;; Copyright © Marc Nieper-Wißkirchen (2020).

;; This file is part of unsyntax.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice (including the
;; next paragraph) shall be included in all copies or substantial
;; portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Syntax Libraries ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *auxiliary-syntax-libraries* '())

(define (install-auxiliary-syntax! libname)
  (set! *auxiliary-syntax-libraries*
    (cons libname *auxiliary-syntax-libraries*)))

(define (auxiliary-syntax-library? libname)
  (and (member libname *auxiliary-syntax-libraries*) #t))

;;;;;;;;;;;;;;;;;;;;
;; Library Finder ;;
;;;;;;;;;;;;;;;;;;;;

(define pending-libraries (make-parameter '()))

(define (%import-library stx magic?)
  (receive (name pred) (parse-library-reference stx)
    (cond ((auxiliary-syntax-library? name)
           (or magic?
               (raise-syntax-error stx "invalid import of library ‘~a’" name)))
          ((member name (pending-libraries))
           (raise-syntax-error stx "circular import of library"))
          (else
           (parameterize ((pending-libraries (cons name (pending-libraries))))
             (find-library stx name pred))))))

(define (library-present? stx)
  (and (%import-library stx #t) #t))

(define (import-library stx)
  (or (%import-library stx #f)
      (raise-syntax-error stx "library ‘~a’ not found" (syntax->datum stx))))

(define (find-library stx name pred)
  (let ((lib
         (call/cc
          (lambda (k)
            (library-table-intern! name
                                   (lambda ()
                                     (or (load-library name pred)
                                         (k #f))))))))
    (when lib
      (unless (pred (library-version lib))
        (raise-syntax-error stx "library ‘~s’ version mismatch" name)))
    lib))

(define (load-library name pred)
  (and-let* ((def (locate-library name pred)))
    (let ((lib (apply expand-library def)))
      (library-bind-globals! lib)
      lib)))

(define bind-global-variable!
  (case-lambda
    ((lbl loc)
     (bind-global-variable! lbl loc #f))
    ((lbl loc lib)
     (bind-global! lbl
                   (make-binding 'global-variable (list lib loc))))))

(define bind-global-keyword!
  (case-lambda
    ((lbl type)
     (bind-global-keyword! lbl type #f))
    ((lbl type lib)
     (bind-global! lbl
                   (if (pair? type)
                       (case (car type)
                         ((meta-variable)
                          (make-meta-binding 'meta-variable (cdr type))))
                       (case type
                         ((property)
                          (make-binding 'global-property (list lib #f)))
                         ((macro macro-parameter)
                          (make-binding 'global-keyword
                                        (list lib type #f)))))))))

(define (library-bind-globals! lib)
  (for-each (lambda (entry)
              (bind-global-variable! (car entry) (cdr entry) lib))
            (library-variables lib))
  (for-each (lambda (entry)
              (bind-global-keyword! (car entry) (cdr entry) lib))
            (library-keywords lib)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Library Installer ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (install-stdlib name version exports keywords vars)
  (library-table-intern!
   name
   (lambda ()
     (make-library name version '() '() '() '() '() #f #f
                   (alist->exports exports)
                   keywords vars))))

(define (install-library name version visreqs invreqs visiter invoker exports
                         keywords vars)
  (library-table-intern!
   name
   (lambda ()
     (let ((lib (make-library name version '() visreqs invreqs '() '() visiter
                              invoker (alist->exports exports) keywords vars)))
       (library-bind-globals! lib)
       lib))))

(define (install-alias! lib alias)
  (library-table-intern! alias (lambda () lib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visit and Invoke Requirements ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-library-collector)
  (let ((libs (make-hash-table eq-comparator)))
    (case-lambda
      (()
       (hash-table-keys libs))
      ((lib)
       (hash-table-set! libs lib #t)))))

(define visit-collector (make-parameter #f))
(define (require-visit! lib) (when lib ((visit-collector) lib)))
(define (visit-requirements) ((visit-collector)))

(define invoke-collector (make-parameter #f))
(define (require-invoke! lib) (when lib ((invoke-collector) lib)))
(define (invoke-requirements) ((invoke-collector)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visiting and Invoking ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (visit-library! lib)
  (let ((name (library-name lib))
        (visiter (library-visiter lib)))
    (when visiter
      (library-set-visiter! lib #t)
      (for-each invoke-library! (library-visit-requirements lib))
      (library-set-visiter! lib
                            (lambda ()
                              (raise-error
                               #f
                               "visit of library ‘~a’ did not return"
                               name)))
      (when (eq? #t visiter)
        (raise-error #f "circular invocation of library ‘~a’" name))
      (visiter)
      (library-set-visiter! lib #f))))

(define (invoke-library! lib)
  (let ((name (library-name lib))
        (invoker (library-invoker lib)))
    (when invoker
      (library-set-invoker! lib #t)
      (for-each invoke-library! (library-invoke-requirements lib))
      (library-set-invoker! lib
                            (lambda ()
                              (raise-error
                               #f
                               "invoke of library ‘~a’ did not return"
                               name)))
      (when (eq? #t invoker)
        (raise-error #f "circular invocation of library ‘~a’" name))
      (invoker)
      (library-set-invoker! lib #f))))

;;;;;;;;;;;;;;;;;;;;;;
;; Library Expander ;;
;;;;;;;;;;;;;;;;;;;;;;

(define (get-props l/p)
  (map (lambda (a)
         (cons (cdr a) 'property))
       (label/props-props l/p)))

(define (get-kwds env)
  (environment-fold/props
   (lambda (id l/p kwds)
     (let ((lbl (label/props-label l/p)))
       (if (auxiliary-syntax-label? lbl) kwds
           (let ((b (current-store-ref lbl)))
             (case (binding-type b)
               ((macro macro-parameter property)
                => (lambda (type)
                     (append (get-props l/p)
                             (alist-cons lbl type kwds))))
               ((meta-variable)
                (append (get-props l/p)
                        (alist-cons lbl (cons 'meta-variable (binding-value b))
                                    kwds)))
               ((lexical)
                (append (get-props l/p) kwds))
               (else kwds))))))
   '() env))

(define (get-vars env)
  (environment-fold (lambda (id lbl vars)
                      (let ((b (current-store-ref lbl)))
                        (case (binding-type b)
                          ((lexical)
                           (alist-cons lbl (binding-value b) vars))
                          (else
                           vars))))
                    '() env))

(define (get-exports export-specs envs)
  (let ((exports (make-exports)))
    (for-each (lambda (spec)
                (export-spec-export! spec envs exports))
              export-specs)
    exports))

(define (expand-library stx name version decls)
  (let*-values
      (((loc) (syntax-object-srcloc stx))
       ((export-specs import-sets body) (parse-library-declarations decls))
       ((import-env) (make-environment))
       ((imported-libs) (environment-import* import-env import-sets)))
    (parameterize ((invoke-collector (make-library-collector))
                   (visit-collector (make-library-collector)))
      (expand-top-level (datum->syntax #f body loc)
                        import-env
                        (lambda (stxdefs defs env)
                          (let-values (((vars) (get-vars env))
                                       ((kwds) (get-kwds env)))
			    (make-library name
                                          version
					  imported-libs
                                          (visit-requirements)
					  (invoke-requirements)
                                          stxdefs
                                          defs
                                          (make-visiter loc stxdefs)
					  (make-initializer loc defs
                                                            (map cdr vars))
					  (get-exports export-specs
						       (list env import-env))
                                          kwds
                                          vars)))))))

(define (make-visiter srcloc stxdefs)
  (let ((body (build-begin srcloc stxdefs)))
    (lambda ()
     (execute body))))

(define (make-initializer srcloc defs locs)
  (let ((body (build-initializer-body srcloc defs locs)))
    (lambda ()
      (execute body))))

(define (build-initializer-body srcloc defs locs)
  (build-body srcloc defs
              (build-begin srcloc
			   (map (lambda (loc)
				  (build-primitive-call
				   srcloc
				   'set-global!
				   (list (build-literal srcloc loc)
					 (build-reference srcloc loc))))
				locs))))

(define (parse-library-reference stx)
  (let ((form (syntax->list stx)))
    (unless form
      (raise-syntax-error stx "ill-formed library reference"))
    (let ((parts (reverse form)))
      (receive (parts ver-ref)
          (if (and (pair? parts)
                   (syntax-pair? (car parts)))
              (values (reverse (cdr parts)) (car parts))
              (values form '()))
        (values (library-name-normalize
                 (map-in-order parse-library-name-part parts))
                (parse-version-reference ver-ref))))))

(define (parse-library-declarations stx*)
  (let f ((stx* stx*))
    (if (null? stx*)
        (values '() '() '())
        (let ((form (syntax->list (car stx*))))
          (unless (pair? form)
            (raise-syntax-error (car stx*) "ill-formed library declaration"))
          (case (syntax-object-expr (car form))
            ((export)
             (receive (exports imports body) (f (cdr stx*))
               (values (append (cdr form) exports) imports body)))
            ((import)
             (receive (exports imports body) (f (cdr stx*))
               (values exports (append (cdr form) imports) body)))
            ((begin)
             (receive (exports imports body) (f (cdr stx*))
               (values exports imports (append (cdr form) body))))
            ((include)
             (let ((included-body (read-file* (cdr form) #f)))
               (receive (exports imports body) (f (cdr stx*))
                 (values exports imports (append included-body body)))))
            ((include-ci)
             (let ((included-body (read-file* (cdr form) #t)))
               (receive (exports imports body) (f (cdr stx*))
                 (values exports imports (append included-body body)))))
            ((include-library-declarations)
             (f (append (read-file* (cdr form) #f) (cdr stx*))))
            ((cond-expand)
             (f (append (eval-cond-expand (car stx*))
                        (cdr stx*))))
            (else
             (raise-syntax-error (car stx*) "invalid library declaration")))))))

;;;;;;;;;;;;;;;;;;;;;
;; Library Exports ;;
;;;;;;;;;;;;;;;;;;;;;

(define (make-exports)
  (make-hash-table eq-comparator))

(define (exports-ref exports name)
  (hash-table-ref/default exports name #f))

(define (exports-set! exports name l/p)
  (hash-table-set! exports name l/p))

(define (exports-for-each proc exports)
  (hash-table-for-each proc exports))

(define exports-map->list hash-table-map->list)

(define exports->alist hash-table->alist)

(define (alist->exports alist) (alist->hash-table alist eq-comparator))

;;;;;;;;;;;;;;;;;;;;;
;; Library Imports ;;
;;;;;;;;;;;;;;;;;;;;;

(define environment-import*
  (case-lambda
    ((env import-specs)
     (environment-import* env import-specs '()))
    ((env import-specs imported-libs)
     (let ((libs (hash-table-unfold null? (lambda (libs)
                                            (values (car libs) #t))
                                    cdr imported-libs eq-comparator)))
       (for-each (lambda (import-spec)
                   (and-let* ((imported-lib
                               (environment-import env import-spec)))
                     (hash-table-set! libs imported-lib #t)))
                 import-specs)
       (hash-table-keys libs)))))

(define (environment-import env import-spec)
  (let* ((import-set (parse-import-spec import-spec))
         (fail (lambda ()
                 (raise-syntax-error import-set "invalid import set")))
         (form (syntax->list import-set)))
    (when (or (not form) (null? form))
      (fail))
    (cond ((library-reference import-set)
           => (lambda (ref)
                (let ((lib (import-library ref)))
                  (unless lib
                    (raise-syntax-error import-set
                                        "library not found ‘~a’"
                                        (syntax->datum import-set)))
                  (environment-add-library-exports! env lib import-set)
                  lib)))
          ((not (identifier? (car form)))
           (fail))
          ;; Magic auxiliary syntax library
          ((and (eq? 'only (identifier-name (car form)))
                (not (null? (cdr form)))
                (syntax-pair? (cadr form))
                (auxiliary-syntax-library? (syntax->datum (cadr form))))
           (import-auxiliary-syntax! (cddr form) env)
           #f)
          (else
           (let ((orig-env (make-environment)))
             (let ((lib (environment-import orig-env (cadr form))))
               (case (identifier-name (car form))
                 ((only)
                  (map-environment-only! (cddr form) orig-env env))
                 ((except)
                  (map-environment-except! import-set (cddr form) orig-env env))
                 ((prefix)
                  (unless (= 3 (length form))
                    (fail))
                  (map-environment-prefix! (caddr form) orig-env env))
                 ((rename)
                  (map-environment-rename! import-set (cddr form) orig-env env))
                 (else
                  (fail)))
               lib))))))

(define (library-reference stx)
  (and-let* ((form (syntax->list stx)))
    (or (and (or (null? form)
                 (null? (cdr form))
                 (not (syntax-pair? (cadr form))))
             stx)
        (and (= 2 (length form))
             (identifier? (car form))
             (eq? 'library (identifier-name (car form)))
             (cadr form)))))

(define (parse-import-spec import-spec)
  (let ((form (syntax->list import-spec))
        (fail (lambda ()
                (raise-syntax-error import-spec "invalid import spec"))))
    (unless form (fail))
    (if (and (pair? form)
             (pair? (cdr form))
             (syntax-pair? (cadr form))
             (identifier? (car form))
             (eq? 'for (identifier-name (car form))))
        (begin
          (unless (pair? (cdr form))
            (fail))
          (for-each parse-import-level (cddr form))
          (cadr form))
        import-spec)))

(define (parse-import-level stx)
  (unless (or (and (identifier? stx)
                   (memq (identifier-name stx) '(run expand)))
              (and-let* ((form (syntax->list stx))
                         ((= 2 (length form)))
                         ((identifier? (car form)))
                         ((eq? 'meta (identifier-name (car form))))
                         ((exact-integer? (syntax->datum (cadr form)))))))
    (raise-syntax-error stx "invalid import import level")))

(define (import-auxiliary-syntax! stx* env)
  (for-each (lambda (id)
              (unless (identifier? id)
                (raise-syntax-error id "identifier expected"))
              (environment-set! env id
                                (auxiliary-syntax-label (identifier-name id))))
            stx*))

(define (map-environment-only! stx* orig-env env)
  (for-each (lambda (id)
	      (unless (identifier? id)
		(raise-syntax-error id "identifier expected"))
	      (let ((l/p (environment-ref/props orig-env id)))
		(environment-set!/props env id l/p)))
	    stx*))

(define (map-environment-except! stx stx* orig-env env)
  (let ((excepted (make-hash-table eq-comparator)))
    (for-each
     (lambda (id)
       (unless (identifier? id)
         (raise-syntax-error id "identifier expected"))
       (let ((name (identifier-name id)))
         (unless (environment-ref orig-env id)
           (raise-syntax-error id
                               "identifier ‘~a’ not found in original set"
                               name))
         (hash-table-update! excepted
                             name
                             values
                             (lambda () #t)
                             (lambda (_)
                               (raise-syntax-error
                                stx
                                "identifier ’~a’ appears twice in subset"
                                name)))))
     stx*)
    (environment-for-each
     (lambda (id l/p)
       (unless (hash-table-ref/default excepted (identifier-name id) #f)
         (environment-set!/props env id l/p)))
     orig-env)))

(define (map-environment-prefix! stx orig-env env)
  (unless (identifier? stx)
    (raise-syntax-error stx "identifier expected"))
  (let ((prefix (symbol->string (identifier-name stx))))
    (environment-for-each
     (lambda (orig-id l/p)
       (let*
           ((orig-name (identifier-name orig-id))
            (name (string->symbol (string-append prefix
                                                 (symbol->string orig-name)))))
         (environment-set!/props env
                                 (datum->syntax orig-id name)
                                 l/p)))
     orig-env)))

(define (map-environment-rename! stx stx* orig-env env)
  (let ((renames (make-hash-table eq-comparator)))
    (for-each
     (lambda (stx)
       (let ((form (syntax->list stx)))
         (unless (and form
                      (= 2 (length form))
                      (identifier? (car form))
                      (identifier? (cadr form)))
           (raise-syntax-error stx "invalid rename"))
         (let ((from (identifier-name (car form)))
               (to (identifier-name (cadr form))))
           (unless (environment-ref orig-env (car form))
             (raise-syntax-error stx
                                 "identifier ‘~a’ not found in original set"
                                 from))
           (hash-table-update! renames
                               from
                               values
                               (lambda () to)
                               (lambda (to)
                                 (raise-syntax-error
                                  stx
                                  "identifier ’~a’ already renamed to ‘~a’"
                                  from
                                  to))))))
     stx*)
    (environment-for-each
     (lambda (orig-id l/p)
       (let* ((orig-name (identifier-name orig-id))
              (name (hash-table-ref/default renames orig-name orig-name)))
         (environment-set!/props env (datum->syntax orig-id name) l/p)))
     orig-env)))

(define (environment-add-library-exports! env lib stx)
  (let ((srcloc (syntax-object-srcloc stx)))
    (exports-for-each
     (lambda (name l/p)
       (environment-set!/props env (datum->syntax #f name srcloc) l/p))
     (library-exports lib))))

;;;;;;;;;;;;;;;;;;
;; Export Specs ;;
;;;;;;;;;;;;;;;;;;

(define (export-spec-export! export-spec envs exports)
  (receive (from* to*) (parse-export-spec export-spec)
    (for-each
     (lambda (from to)
       (let ((from-name (identifier-name from))
             (to-name (identifier-name to)))
         (when (exports-ref exports to-name)
           (raise-syntax-error to "identifier exported twice ‘~a’" to-name))
         (let f ((envs envs))
           (if (null? envs)
               (raise-syntax-error from
                                   "trying to export unbound identifier ‘~a’"
                                   from-name)
               (cond
                ((environment-ref/props (car envs) from)
                 => (lambda (l/p)
                      (exports-set! exports to-name l/p)))
                (else
                 (f (cdr envs))))))))
     from* to*)))

(define (parse-export-spec spec)
  (if (identifier? spec)
      (values (list spec) (list spec))
      (let ((form (syntax->list spec))
            (fail (lambda ()
                    (raise-syntax-error spec "invalid export spec"))))
        (unless (and form
                     (pair? form)
                     (eq? 'rename (syntax->datum (car form))))
          (fail))
        (if (and (= 3 (length form))
                 (identifier? (cadr form))
                 (identifier? (caddr form)))
            (values (list (cadr form)) (list (caddr form)))
            (let f ((form (cdr form)) (from* '()) (to* '()))
              (if (null? form)
                  (values (reverse! from*) (reverse! to*))
                  (let ((e (syntax->list (car form))))
                    (unless (and e (= 2 (length e))
                                 (identifier? (car e))
                                 (identifier? (cadr e)))
                      (fail))
                    (f (cdr form)
                       (cons (car e) from*)
                       (cons (cadr e) to*)))))))))
