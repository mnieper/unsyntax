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

(define (%import-library stx)
  (receive (name pred) (parse-library-reference stx)
    (cond ((auxiliary-syntax-library? name)
           'auxiliary-syntax-library)
          ((member name (pending-libraries))
           (raise-syntax-error stx "circular import of library"))
          (else
           (parameterize ((pending-libraries (cons name (pending-libraries))))
             (find-library stx name pred))))))

(define (library-present? stx)
  (and (%import-library stx) #t))

(define (import-library stx)
  (or (%import-library stx)
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
      (record-expanded-library! lib)
      (library-bind-globals! lib)
      lib)))

(define create-global-binding!
  (case-lambda
    ((lbl binding)
     (create-global-binding! lbl binding #f))
    ((lbl binding lib)
     (bind-global!
      lbl
      (case (car binding)
        ((variable)
         (make-binding 'global-variable (list lib (cadr binding))))
        ((meta-variable)
         (make-meta-binding 'meta-variable (cdr binding)))
        ((property)
         (make-binding 'global-property (list lib #f)))
        ((macro macro-parameter) =>
         (lambda (type)
           (make-binding 'global-keyword (list lib type #f))))
        ((module)
         (make-binding 'module (cdr binding)))
        (else (error "create-global-binding!: invalid binding" binding)))))))

(define (library-bind-globals! lib)
  (for-each (lambda (entry)
              (create-global-binding! (car entry) (cdr entry) lib))
            (library-bindings lib)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Library Installer ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (install-stdlib name version exports bindings)
  (library-table-intern!
   name
   (lambda ()
     (make-library name version '() '() '() '() #f #f
                   exports
                   bindings))))

(define (install-library name version visreqs invreqs visiter invoker exports
                         bindings)
  (library-table-intern!
   name
   (lambda ()
     (let ((lib (make-library name version visreqs invreqs '() '() visiter
                              invoker exports bindings)))
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

(define (get-exports export-specs rib)
  (let ((exports (make-identifier-table)))
    (for-each (lambda (spec)
                (export-spec-export! spec rib exports))
              export-specs)
    (identifier-table->export-set '() exports)))

(define (expand-library stx name version decls)
  (let*-values
      (((loc) (syntax-object-srcloc stx))
       ((export-specs import-sets body) (parse-library-declarations decls))
       ((import-env) (make-rib)))
    (rib-import* import-env import-sets)
    (parameterize ((invoke-collector (make-library-collector))
                   (visit-collector (make-library-collector)))
      (expand-top-level (datum->syntax #f body loc)
                        import-env
                        (lambda (bindings stxdefs defs env)
			  (make-library name
                                        version
                                        (visit-requirements)
                                        (invoke-requirements)
                                        stxdefs
                                        defs
                                        (make-visiter loc stxdefs)
                                        (make-initializer loc defs bindings)
                                        (get-exports export-specs env)
                                        bindings))))))

(define (make-visiter srcloc stxdefs)
  (let ((body (build-begin srcloc stxdefs)))
    (lambda ()
     (execute body))))

(define (make-initializer srcloc defs bindings)
  (define locs
    (filter-map (lambda (label+binding)
                  (case (cadr label+binding)
                    ((variable)
                     (caddr label+binding))
                    (else
                     #f)))
                bindings))
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

(define (split-library-reference stx)
  (let ((form (syntax->list stx)))
    (unless form
      (raise-syntax-error stx "ill-formed library reference"))
    (let ((parts (reverse form)))
      (if (and (pair? parts)
               (syntax-pair? (car parts)))
          (values (reverse (cdr parts)) (car parts))
          (values form '())))))

(define (parse-library-reference stx)
  (define-values (parts ver-ref) (split-library-reference stx))
  (values (library-name-normalize
           (map-in-order parse-library-name-part parts))
          (parse-version-reference ver-ref)))

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
;; Library Imports ;;
;;;;;;;;;;;;;;;;;;;;;

(define (rib-import* env import-specs)
  (for-each (lambda (import-spec)
              (rib-import env import-spec))
            import-specs))

(define (rib-import rib import-spec)
  (define-values (import-set module-id) (expand-import-spec import-spec #f))
  (define loc (syntax-object-srcloc module-id))
  (define substs (syntax-object-substs module-id))
  (define (import! exported-id l/p)
    (define id (make-syntax-object (identifier-name exported-id)
                                   (syntax-object-marks exported-id)
                                   substs
                                   loc))
    (rib-set!/props rib id l/p))
  (import-set-for-each import! import-set))

;;; Parse an import-spec and return two values, the resulting
;;; import-set and the identifier denoting the module or library
;;; reference.
(define (expand-import-spec import-spec local?)
  (define k (make-identifier #f '()))
  (define import-set-form (parse-import-spec import-spec))
  (define-values (import-set mid)
    (let f ((import-set-form import-set-form))
      (define (fail)
        (raise-syntax-error import-set-form "invalid import set"))
      (define form (syntax->list import-set-form))
      (cond
       ;; <module name>
       ((and local? (identifier? import-set-form))
        (let ((lbl (resolve import-set-form)))
          (define b (lookup lbl))
          (unless lbl
            (raise-syntax-error import-set-form "identifier ‘~a’ unbound"
                                (identifier-name import-set-form)))
          (case (binding-type b)
            ((module)
             (values (export-set->import-set import-set-form (binding-value b))
                     import-set-form))
            (else
             (raise-syntax-error
              import-set-form "identifier ‘~a’ does not name a module"
              (identifier-name import-set-form))))))
       ((not form) (fail))
       ;; library
       ((and (not local?) (library-reference import-set-form))
        => (lambda (ref)
             (let ((lib (import-library ref)))
               (if (eq? 'auxiliary-syntax-library lib)
                   (values 'auxiliary-syntax
                           k)
                   (values (export-set->import-set k (library-exports lib))
                           k)))))
       ((null? form) (fail))
       ((not (identifier? (car form))) (fail))
       ;; only
       ((eq? 'only (identifier-name (car form)))
        (check-identifiers (cddr form))
        (receive (import-set mid) (f (cadr form))
          (values (if (eq? 'auxiliary-syntax import-set)
                      (import-set-auxiliary-syntax mid (cddr form))
                      (import-set-only import-set (cddr form)))
                  mid)))
       ;; except
       ((eq? 'except (identifier-name (car form)))
        (check-identifiers (cddr form))
        (receive (import-set mid) (f (cadr form))
          (check-not-auxiliary-syntax import-set-form import-set)
          (values (import-set-except import-set (cddr form))
                  mid)))
       ;; prefix
       ((eq? 'prefix (identifier-name (car form)))
        (unless (= 3 (length form)) (fail))
        (check-identifiers (cddr form))
        (receive (import-set mid) (f (cadr form))
          (check-not-auxiliary-syntax import-set-form import-set)
          (values (import-set-prefix import-set (caddr form))
                  mid)))
       ;; rename
       ((eq? 'rename (identifier-name (car form)))
        (let g ((stx* (cddr form)) (from-id* '()) (to-id* '()))
          (if (null? stx*)
              (receive (import-set mid) (f (cadr form))
                (check-not-auxiliary-syntax import-set-form import-set)
                (values (import-set-rename import-set (reverse! from-id*)
                                           (reverse! to-id*))
                        mid))
              (receive (from-id to-id) (parse-rename (car stx*))
                (g (cdr stx*)
                   (cons from-id from-id*)
                   (cons to-id to-id*))))))
       ((not local?) (fail))
       ;; library
       (else
        (receive (ref mid)
            (local-library-reference import-set-form)
          (let ((lib (import-library ref)))
            (if (eq? 'auxiliary-syntax-library lib)
                (values 'auxiliary-syntax
                        mid)
                (values (export-set->import-set mid (library-exports lib))
                        mid))))))))
  (check-not-auxiliary-syntax import-set-form import-set)
  (values import-set mid))

(define (check-identifiers stx*)
  (for-each (lambda (stx)
              (unless (identifier? stx)
                (raise-syntax-error stx "identifier expected")))
            stx*))

(define (check-not-auxiliary-syntax stx import-set)
  (when (eq? 'auxiliary-syntax import-set)
    (raise-syntax-error stx "invalid import of auxiliary syntax")))

(define (library-reference stx)
  (and-let* ((form (syntax->list stx))
             ((pair? form)))
    (or (and (or (null? (cdr form))
                 (not (syntax-pair? (cadr form))))
             stx)
        (and (= 2 (length form))
             (identifier? (car form))
             (eq? 'library (identifier-name (car form)))
             (cadr form)))))

(define (local-library-reference stx)
  (define form (syntax->list stx))
  (if (and (= 2 (length form))
           (identifier? (car form))
           (eq? 'library (identifier-name (car form))))
      (values (cadr form) (car form))
      (receive (name version-ref) (split-library-reference stx)
        (define last (list-ref name (- (length name) 1)))
        (values stx (if (identifier? last) last
                        (datum->syntax #f 'library))))))

(define (parse-import-spec import-spec)
  (if (identifier? import-spec) import-spec
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
            import-spec))))

(define (parse-import-level stx)
  (unless (or (and (identifier? stx)
                   (memq (identifier-name stx) '(run expand)))
              (and-let* ((form (syntax->list stx))
                         ((= 2 (length form)))
                         ((identifier? (car form)))
                         ((eq? 'meta (identifier-name (car form))))
                         ((exact-integer? (syntax->datum (cadr form)))))))
    (raise-syntax-error stx "invalid import import level")))

(define (parse-rename stx)
  (define form (syntax->list stx))
  (unless (and form
               (= 2 (length form))
               (identifier? (car form))
               (identifier? (cadr form)))
    (raise-syntax-error stx "invalid rename"))
  (values (car form) (cadr form)))

;;;;;;;;;;;;;;;;;;
;; Export Specs ;;
;;;;;;;;;;;;;;;;;;

(define (export-spec-export! export-spec rib exports)
  (receive (from* to*) (parse-export-spec export-spec)
    (for-each
     (lambda (from to)
       (let ((from-name (identifier-name from))
             (to-name (identifier-name to)))
         (when (identifier-table-ref exports to)
           (raise-syntax-error to "identifier exported twice ‘~a’" to-name))
         (cond
	  ((resolve/props (add-substs rib from))
	   => (lambda (l/p)
		(identifier-table-set! exports to l/p)))
	  (else
	   (raise-syntax-error from
			       "trying to export unbound identifier ‘~a’"
			       from-name)))))
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
