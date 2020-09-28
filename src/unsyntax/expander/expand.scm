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

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;

(define (genvar id) (generate-variable (identifier-name id)))

(define (genlbl id) (make-label))

(define (genref id)
  (and id (build-reference (syntax-object-srcloc id) (genvar))))

(define (make-lexical-binding var) (make-binding 'lexical var))

(define (valid-bound-identifiers? stx*)
  (let ((env (make-environment)))
    (for-each (lambda (stx)
                (unless (identifier? stx)
                  (raise-syntax-error stx "identifier expected"))
                (environment-update! env
                                     stx
                                     values
                                     (lambda () #f)
                                     (lambda (val)
                                       (raise-syntax-error
                                        stx
                                        "duplicate identifier ‘~a’ bound"
                                        (identifier-name stx)))))
              stx*)))

;;;;;;;;;;;;;;;;;
;; Syntax Type ;;
;;;;;;;;;;;;;;;;;

;;; Return #t if the datum E is self-evaluationg, #f otherwise.
(define (self-evaluating? e)
  (or (boolean? e)
      (bytevector? e)
      (char? e)
      (number? e)
      (string? e)
      (vector? e)))

(define (make-kwd-table) (make-hash-table eq-comparator))

(define (kwd-table-add! table id lbl)
  (and table
       (let ((entry (make-kwd-entry id lbl)))
         (hash-table-update!/default table
                                     (identifier-name id)
                                     (lambda (entries)
                                       (lset-adjoin kwd-entry=?
                                                    entries
                                                    (make-kwd-entry id lbl)))
                                     '()))))

(define (kwd-table-contains? table id lbl)
  (and table
       (hash-table-ref table
                       (identifier-name id)
                       (lambda () #f)
                       (lambda (entries)
                         (and (member (make-kwd-entry id lbl)
                                      entries
                                      kwd-entry=?)
                              #t)))))

(define (make-kwd-entry id lbl) (cons (syntax-object-marks id) lbl))

(define (kwd-entry=? entry1 entry2)
  (and (marks=? (car entry1) (car entry2))
       (label=? (cdr entry1) (cdr entry2))))

(define (syntax-type stx env)
  (cond
   ((syntax-pair? stx)
    (let ((h (syntax-car stx)))
      (cond
       ((identifier? h)
        (let* ((lbl (resolve h))
               (b (lookup lbl)))
          (kwd-table-add! (current-keywords) h lbl)
          (case (binding-type b)
            ((alias begin define-auxiliary-syntax define-record-type
              define-values define-syntax define-syntax-parameter import
              let-syntax letrec-syntax with-ellipsis)
             => (lambda (type)
                  (values stx type #f)))
            ((core)
             (values stx 'core (binding-value b)))
            ((macro macro-parameter)
             (syntax-type (transform/macro (binding-value b)
                                           stx env) env))
            ((global-keyword)
             (syntax-type (transform/global-keyword (binding-value b) stx env)
                          env))
            (else
             (values stx 'call #f)))))
       (else
        (values stx 'call #f)))))
   (else
    (cond
     ((identifier? stx)
      (cond
       ((resolve stx)
        => (lambda (lbl)
             (let ((b (lookup lbl)))
               (kwd-table-add! (current-keywords) stx lbl)
               (case (binding-type b)
                 ((macro macro-parameter)
                  (syntax-type (transform/macro (binding-value b)
                                                stx env) env))
                 ((global-keyword)
                  (syntax-type (transform/global-keyword (binding-value b)
                                                         stx env)
                               env))
                 ((#f)
                  (raise-syntax-error stx
                                      "displaced identifier ‘~a’"
                                      (identifier-name stx)))
                 (else
                  => (lambda (t)
                       (values stx t (binding-value b))))))))
       (else
        (raise-syntax-error
         stx "unbound identifier ‘~a’" (identifier-name stx)))))
     (else
      (let ((datum (syntax->datum stx)))
        (cond ((self-evaluating? datum)
               (values stx 'literal datum))
              ((capture-syntactic-environment? datum)
               (syntax-type ((capture-syntactic-environment-procedure datum)
                             (datum->syntax stx #f))
                            env))
              (else
               (values stx 'other #f)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special Identifiers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-ellipsis-identifier-name #f)
(define (ellipsis-identifier id)
  (datum->syntax id the-ellipsis-identifier-name))

;;;;;;;;;;;;;;
;; Expander ;;
;;;;;;;;;;;;;;

(define current-top-level? (make-parameter #f))
(define current-keywords (make-parameter #f))

(define (expand-top-level stx import-env k)
  (with-frame '() '()
    (receive (stxdefs defs expr env)
        (expand-internal (add-substs import-env stx) import-env)
      (k stxdefs defs env))))

(define (expand-internal stx import-env)
  (parameterize ((current-top-level? (and import-env #t)))
    (letrec*
        ((env (make-environment))
         (body (add-substs* env stx))
         (add! (lambda (id lbl)
                 (let ((name (identifier-name id)))
                   (when (and import-env (environment-ref import-env id))
                     (raise-syntax-error
                      id
                      "trying to redefine imported identifier ‘~a’"
                      name))
                   (when (kwd-table-contains? (current-keywords)
                                              id (resolve id))
                     (raise-syntax-error
                      id
                      "trying to define an identifier ‘~a’ whose binding has
 already been used"
                      name))
                   (when id
                     (environment-set! env id lbl))))))
      (receive (stxdefs defs expr)
          (parameterize ((current-keywords (make-kwd-table)))
            (expand-form* body '() '() env add!))
        (unless stxdefs (raise-syntax-error stx "no expressions in body"))
        (values stxdefs defs expr env)))))

(define (expand-definitions rdefs)
  (map (lambda (def) (if (procedure? def) (def) def))
       (reverse! rdefs)))

(define (expand-form* stx* stxdef* rdef* env add!)
  (if (null? stx*)
      (if (current-top-level?)
          (values (reverse! stxdef*) (expand-definitions rdef*) #f)
          (values #f #f #f))
      (expand-form (car stx*) (cdr stx*) stxdef* rdef* env add!)))

(define (expand-form stx stx* stxdef* rdef* env add!)
  (let ((loc (syntax-object-srcloc stx)))
    (receive (stx type val) (syntax-type stx env)
      (case type
        ((alias)
         (let*-values (((id1 id2) (parse-alias stx))
                       ((lbl) (resolve id2)))
           (kwd-table-add! (current-keywords) id2 lbl)
           (add! id1 lbl)
           (expand-form* stx* stxdef* rdef* env add!)))
        ((begin)
         (expand-form* (append (parse-begin stx #f) stx*)
                       stxdef* rdef* env add!))
        ((define-auxiliary-syntax)
         (receive (id sym) (parse-define-auxiliary-syntax stx)
           (add! id (auxiliary-syntax-label sym))
           (expand-form* stx* stxdef* rdef* env add!)))
        ((define-record-type)
         (expand-form* stx*
                       stxdef*
                       (cons (expand-define-record-type stx add!) rdef*)
                       env add!))
        ((define-syntax define-syntax-parameter)
         (expand-form* stx*
                       (cons (expand-define-syntax type stx add!) stxdef*)
                       rdef* env add!))
        ((define-values)
         (expand-form* stx*
                       stxdef*
                       (cons (expand-define-values stx add!) rdef*)
                       env add!))
        ((let-syntax letrec-syntax)
         (expand-form* (append (expand-let-syntax* type stx) stx*)
                       stxdef*
                       rdef*
                       env add!))
        ((with-ellipsis)
         (expand-form* (append (expand-with-ellipsis* stx) stx*)
                       stxdef*
                       rdef*
                       env add!))
        (else
         (if (current-top-level?)
             (expand-form* stx*
                           stxdef*
                           (cons (lambda ()
                                   (build-command loc (expand stx)))
                                 rdef*)
                           env add!)
             (values (reverse! stxdef*)
                     (expand-definitions rdef*)
                     (build-begin loc
                                  (parameterize ((current-keywords #f))
                                    (map expand (cons stx stx*)))))))))))

(define (expand-let-syntax* type stx)
  (let*-values (((ids inits body) (parse-let-syntax stx #f))
                ((lbls) (map genlbl ids))
                ((env) (make-environment ids lbls)))
    (for-each (lambda (id lbl init)
                (let ((b (make-transformer-binding
                          'define-syntax
                          (expand-transformer
                           id
                           (if (eq? 'let-syntax type)
                               init
                               (add-substs env init))))))
                  (bind! lbl b)
                  (bind-meta! lbl b)))
              ids lbls inits)
    (add-substs* env body)))

(define (expand-with-ellipsis* stx)
  (let*-values (((id body) (parse-with-ellipsis stx #f))
                ((lbl) (genlbl id)))
    (bind! lbl id)
    (bind-meta! lbl id)
    (add-substs* (make-environment (list (ellipsis-identifier id))
                                   (list lbl))
                 body)))

(define (expand-define-record-type stx add!)
  (receive (rtd cname pred cfields names accs muts)
      (parse-define-record-type stx)
    (let ((indices (get-field-indices names cfields))
          (cvar (genvar cname))
          (pvar (genvar pred))
          (avars (map genvar accs))
          (mvars (map (lambda (mut) (and mut (genvar mut))) muts))
          (rlbl (genlbl rtd))
          (clbl (genlbl cname))
          (plbl (genlbl pred))
          (albls (map genlbl accs))
          (mlbls (map (lambda (mut) (and mut (genlbl mut))) muts)))
      (add! rtd rlbl)
      (add! cname clbl)
      (add! pred plbl)
      (for-each add! accs albls)
      (for-each (lambda (mut mlbl)
                  (when mut
                    (add! mut mlbl)))
                muts mlbls)
      (bind! rlbl (make-binding 'record-type-descriptor #f))
      (bind! clbl (make-lexical-binding cvar))
      (bind! plbl (make-lexical-binding pvar))
      (for-each (lambda (albl avar)
                  (bind! albl (make-lexical-binding avar)))
                albls avars)
      (for-each (lambda (mlbl mvar)
                  (when mlbl
                    (bind! mlbl (make-lexical-binding mvar))))
                mlbls mvars)
      (build-define-record-type
       (syntax-object-srcloc stx) (identifier-name rtd)
       (build-reference (syntax-object-srcloc cname) cvar)
       (build-reference (syntax-object-srcloc pred) pvar)
       indices (map identifier-name names)
       (map (lambda (acc avar)
              (build-reference (syntax-object-srcloc acc) avar))
            accs avars)
       (map (lambda (mut mvar)
              (and mut
                   (build-reference (syntax-object-srcloc mut) mvar)))
            muts mvars)))))

(define (get-field-indices names cfields)
  (map (lambda (cfield)
         (let ((same? (lambda (name) (bound-identifier=? cfield name))))
           (or (list-index same? names)
               (raise-syntax-error
                cfield
                "unknown constructor field name ‘~a’"
                (identifier-name cfield)))))
       cfields))

(define (expand-define-syntax type stx add!)
  (let*-values (((srcloc) (syntax-object-srcloc stx))
                ((id init) (parse-define-syntax stx))
                ((def) (expand-transformer id init))
                ((lbl) (genlbl id))
                ((var) (genvar id))
                ((b) (make-transformer-binding type def)))
    (add! id lbl)
    (bind! lbl b)
    (bind-meta! lbl b)
    (build srcloc (set-keyword! ',lbl (list ,(car def) ',(cadr def))))))

(define (expand-define-values stx add!)
  (let*-values (((formals init) (parse-define-values stx))
                ((ids variadic?) (parse-formals formals))
                ((lbls) (map genlbl ids))
                ((vars) (map genvar ids))
                ((refs) (map build-reference
                             (map syntax-object-srcloc ids)
                             vars))
                ((bs) (map make-lexical-binding vars)))
    (for-each add! ids lbls)
    (for-each bind! lbls bs)
    (lambda ()
      (build-define-values (syntax-object-srcloc stx)
                           (build-formals (syntax-object-srcloc formals)
                                          refs
                                          variadic?)
                           (expand init)))))

(define (expand stx)
  (let*-values (((stx type val) (syntax-type stx #f))
                ((loc) (syntax-object-srcloc stx)))
    (case type
      ((begin stx)
       (expand-begin stx))
      ((call)
       (expand-call stx))
      ((core)
       (val stx))
      ((global-variable)
       (require-invoke! (car val))
       (build-reference loc (cadr val)))
      ((lexical)
       (build-reference loc val))
      ((literal)
       (build-literal loc val))
      ((let-syntax letrec-syntax)
       (expand-let-syntax type stx))
      ((mutable-variable)
       (build-global-reference loc val))
      ((primitive)
       (build-primitive loc val))
      ((syntax)
       (raise-syntax-error
        stx "reference to pattern variable ‘~a’ outside syntax template"
        (identifier-name stx)))
      ((with-ellipsis)
       (expand-with-ellipsis stx))
      (else
       (raise-syntax-error stx "invalid expression type ")))))

(define (expand-body stx env)
  (with-frame '() '()
    (receive (stxdefs defs expr inner-env)
        (expand-internal
         (datum->syntax #f (add-substs* env stx) (syntax-object-srcloc stx)) #f)
      (build-body (syntax-object-srcloc stx) defs expr))))

(define (expand-begin stx)
  (build-begin (syntax-object-srcloc stx)
               (map expand (parse-begin stx #t))))

(define (expand-call stx)
  (receive (operator operands) (parse-call stx)
    (build-call (syntax-object-srcloc stx)
                (expand operator)
                (map expand operands))))

(define (expand-let-syntax type stx)
  (let*-values (((ids inits body) (parse-let-syntax stx #t))
                ((lbls) (map genlbl ids))
                ((env) (make-environment ids lbls)))
    (with-meta-frame lbls
        (map (lambda (id init)
               (make-transformer-binding 'define-syntax
                                         (expand-transformer
                                          id
                                          (if (eq? 'let-syntax type)
                                              init
                                              (add-substs env init)))))
	     ids inits)
      (expand-body body env))))

(define (expand-with-ellipsis stx)
  (let*-values (((id body) (parse-with-ellipsis stx #t))
                ((lbl) (genlbl id)))
    (with-meta-frame (list lbl) (list id)
      (expand-body body (make-environment (list (ellipsis-identifier id))
                                          (list lbl))))))

;;;;;;;;;;;;;
;; Parsers ;;
;;;;;;;;;;;;;

(define (parse-alias stx)
  (let ((fail
         (lambda ()
           (raise-syntax-error stx "ill-formed alias definition")))
        (form (syntax->list stx)))
    (unless (and form (= 3 (length form))) (fail))
    (let ((id1 (cadr form)) (id2 (caddr form)))
      (unless (and (identifier? id1) (identifier? id2)) (fail))
      (values id1 id2))))

(define (parse-begin stx non-empty?)
  (let ((form (syntax->list stx)))
    (unless form
      (raise-syntax-error stx "ill-formed begin"))
    (when (and non-empty? (null? (cdr form)))
      (raise-syntax-error stx "empty begin expression"))
    (cdr form)))

(define (parse-call stx)
  (let ((form (syntax->list stx)))
    (unless (and form (pair? form))
      (raise-syntax-error stx "ill-formed procedure call"))
    (values (car form) (cdr form))))

(define (parse-define-auxiliary-syntax stx)
  (let ((fail
         (lambda ()
           (raise-syntax-error stx "ill-formed auxiliary syntax definition")))
        (form (syntax->list stx)))
    (unless (and form (= 3 (length form))) (fail))
    (let ((id (cadr form)) (sym (caddr form)))
      (unless (and (identifier? id) (identifier? sym)) (fail))
      (values id (identifier-name sym)))))

(define (parse-define-record-type stx)
  (let ((fail
         (lambda ()
           (raise-syntax-error stx "ill-formed record-type definition")))
        (form (syntax->list stx)))
    (unless (and form (<= 4 (length form))) (fail))
    (let ((rtd (cadr form))
          (constructor (syntax->list (caddr form)))
          (pred (cadddr form)))
      (unless (and (identifier? rtd)
                   constructor
                   (every identifier? constructor)
                   (identifier? pred))
        (fail))
      (let*-values (((names accs muts) (parse-record-fields (cddddr form)))
                    ((cfields) (cdr constructor)))
        (unless (valid-bound-identifiers? names)
          (raise-syntax-error stx "invalid record-type field names"))
        (unless (valid-bound-identifiers? cfields)
          (raise-syntax-error stx "invalid constructor field names"))
        (values rtd (car constructor) pred cfields names accs muts)))))

(define (parse-record-fields fields)
  (let f ((fields fields))
    (if (null? fields)
        (values '() '() '())
        (let-values (((name acc mut) (parse-record-field (car fields)))
                     ((names accs muts) (f (cdr fields))))
          (values (cons name names)
                  (cons acc accs)
                  (cons mut muts))))))

(define (parse-record-field field)
  (let ((form (syntax->list field)))
    (unless (and form (<= 2 (length form) 3) (every identifier? form))
      (raise-syntax-error field "ill-formed record-type field"))
    (if (= 2 (length form))
        (values (car form) (cadr form) #f)
        (values (car form) (cadr form) (caddr form)))))

(define (parse-define-syntax stx)
  (let ((form (syntax->list stx)))
    (unless (and form
		 (= 3 (length form))
		 (identifier? (cadr form)))
      (raise-syntax-error stx "ill-formed syntax definition"))
    (values (cadr form) (caddr form))))

(define (parse-define-values stx)
  (let ((form (syntax->list stx)))
    (unless (and form
		 (= 3 (length form)))
      (raise-syntax-error stx "ill-formed definition"))
    (values (cadr form) (caddr form))))

(define (parse-formals formals)
  (receive (ids variadic?)
      (let f ((formals formals))
        (cond
         ((syntax-pair? formals)
          (receive (ids variadic?) (f (syntax-cdr formals))
            (values (cons (syntax-car formals) ids) variadic?)))
         ((syntax-null? formals)
          (values '() #f))
         (else
          (values (list formals) #t))))
    (unless (valid-bound-identifiers? ids)
      (raise-syntax-error formals "invalid formals"))
    (values ids variadic?)))

(define (parse-let-syntax stx non-empty?)
  (let ((fail
         (lambda ()
           (raise-syntax-error stx "ill-formed let(rec)-syntax form")))
        (form (syntax->list stx)))
    (unless (and form (<= 2 (length form))) (fail))
    (when (and non-empty? (null? (cddr form)))
      (raise-syntax-error stx "empty let(rec) expression"))
    (let ((bindings (syntax->list (cadr form))))
      (unless bindings (fail))
      (receive (ids inits)
          (let f ((bindings bindings))
            (if (null? bindings)
                (values '() '())
                (let-values (((ids inits) (f (cdr bindings)))
                             ((form) (syntax->list (car bindings))))
                  (unless (and form (= 2 (length form))) (fail))
                  (values (cons (car form) ids)
                          (cons (cadr form) inits)))))
        (unless (valid-bound-identifiers? ids) (fail))
        (values ids inits (syntax-cdr (syntax-cdr stx)))))))

(define (parse-with-ellipsis stx non-empty?)
  (let ((form (syntax->list stx)))
    (unless (and form (<= (if non-empty? 3 2) (length form)))
      (raise-syntax-error stx "ill-formed with-ellipsis form" stx))
    (values (cadr form) (syntax-cdr (syntax-cdr stx)))))
