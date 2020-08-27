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

;;;;;;;;;;;;;;;;;
;; Case Lambda ;;
;;;;;;;;;;;;;;;;;

(define-core-form! 'case-lambda
  (lambda (stx)
    (let*-values (((formals-list body-list) (parse-case-lambda stx))
                  ((clauses)
                   (map (lambda (formals body)
                          (receive (formals body)
                              (expand-lambda-clause formals body)
                            (list formals body))
                          )
                        formals-list body-list)))
      (build-case-lambda (syntax-object-srcloc stx)
                         (map car clauses)
                         (map cadr clauses)))))

(define (parse-case-lambda stx)
  (let ((form (syntax->list stx)))
    (unless form
      (raise-syntax-error stx "ill-formed case-lambda"))
    (for-each (lambda (stx)
                (unless (syntax-pair? stx)
                  (raise-syntax-error stx "ill-formed case-lambda clause")))
              (cdr form))
    (values (map syntax-car (cdr form))
            (map syntax-cdr (cdr form)))))

(define (expand-lambda-clause formals body)
  (let*-values (((ids variadic?) (parse-formals formals))
                ((lbls) (map genlbl ids))
                ((vars) (map genvar ids)))
    (values (build-formals (syntax-object-srcloc formals)
                           (map build-reference
                                (map syntax-object-srcloc ids)
                                vars)
                           variadic?)
            (with-frame lbls (map make-lexical-binding vars)
              (expand-body body (make-environment ids lbls))))))

;;;;;;;;;;;;;;;;;;
;; Conditionals ;;
;;;;;;;;;;;;;;;;;;

(define-core-form! 'if
  (lambda (stx)
    (receive (test consequent alternate) (parse-conditional stx)
      (apply build-conditional (syntax-object-srcloc stx)
             (expand test)
             (expand consequent)
             (if alternate
                 (list (expand alternate))
                 '())))))

(define (parse-conditional stx)
  (let ((form (syntax->list stx)))
    (unless (and form (<= 3 (length form) 4))
      (raise-syntax-error stx "ill-formed conditional"))
    (case (length form)
      ((3)
       (values (cadr form) (caddr form) #f))
      ((4)
       (values (cadr form) (caddr form) (cadddr form))))))

;;;;;;;;;;;;
;; Letrec ;;
;;;;;;;;;;;;

(define (letrec-expander kwd)
  (lambda (stx)
    (let*-values (((ids inits body) (parse-letrec stx))
                  ((lbls) (map genlbl ids))
                  ((vars) (map genvar ids))
                  ((env) (make-environment ids lbls)))
      (with-frame lbls (map make-lexical-binding vars)
        ((case kwd
            ((letrec) build-letrec)
            ((letrec*) build-letrec*))
         (syntax-object-srcloc stx)
         (map build-reference
              (map syntax-object-srcloc ids)
              vars)
         (map (lambda (init)
                (expand (add-substs env init)))
              inits)
         (expand-body body env))))))

(define-core-form! 'letrec (letrec-expander 'letrec))
(define-core-form! 'letrec* (letrec-expander 'letrec*))

(define (parse-letrec stx)
  (let ((fail (lambda ()
                (raise-syntax-error stx "ill-formed letrec form")))
        (form (syntax->list stx)))
    (unless (and form (<= 2 (length form))) (fail))
    (let ((bindings (syntax->list (cadr form))))
      (unless bindings (fail))
      (receive (ids inits)
          (let f ((bindings bindings))
            (if (null? bindings)
                (values '() '())
                (receive (ids inits) (f (cdr bindings))
                  (let ((form (syntax->list (car bindings))))
                    (unless (and form (= 2 (length form)))
                      (fail))
                    (values (cons (car form) ids)
                            (cons (cadr form) inits))))))
        (unless (valid-bound-identifiers? ids) (fail))
        (values ids inits (syntax-cdr (syntax-cdr stx)))))))

;;;;;;;;;;;;;;;;
;; Let-Values ;;
;;;;;;;;;;;;;;;;

(define-core-form! 'let-values
  (lambda (stx)
    (let*-values (((formals-list ids-list variadic?-list inits body)
                   (parse-let-values stx))
                  ((vars-list) (map (lambda (ids) (map genvar ids)) ids-list))
                  ((ids) (concatenate ids-list))
                  ((lbls) (map genlbl ids))
                  ((env) (make-environment ids lbls)))
      (with-frame lbls
          (append-map (lambda (vars) (map make-lexical-binding vars)) vars-list)
        (build-let-values (syntax-object-srcloc stx)
                          (map (lambda (formals ids vars variadic?)
                                 (build-formals
                                  (syntax-object-srcloc formals)
                                  (map (lambda (id var)
                                         (build-reference
                                          (syntax-object-srcloc id) var))
                                       ids vars)
                                  variadic?))
                               formals-list ids-list vars-list variadic?-list)
                          (map expand inits)
                          (expand-body body env))))))

(define (parse-let-values stx)
  (let ((fail
         (lambda ()
           (raise-syntax-error stx "ill-formed let-values form")))
        (form (syntax->list stx)))
    (unless (and form (<= 2 (length form))) (fail))
    (let ((bindings (syntax->list (cadr form))))
      (unless bindings (fail))
      (receive (formals-list ids-list variadic?-list inits)
          (let f ((bindings bindings))
            (if (null? bindings)
                (values '() '() '() '())
                (let-values (((formals-list ids-list variadic?-list inits)
                              (f (cdr bindings)))
                             ((form) (syntax->list (car bindings))))
                  (unless (and form (= 2 (length form))) (fail))
                  (receive (ids variadic?) (parse-formals (car form))
                    (values (cons (car form) formals-list)
                            (cons ids ids-list)
                            (cons variadic? variadic?-list)
                            (cons (cadr form) inits))))))
        (unless (valid-bound-identifiers? (concatenate ids-list)) (fail))
        (values formals-list ids-list variadic?-list inits
                (syntax-cdr (syntax-cdr stx)))))))

;;;;;;;;;;;;;;;;;;
;; Parameterize ;;
;;;;;;;;;;;;;;;;;;

(define-core-form! 'parameterize
  (lambda (stx)
    (receive (params inits body) (parse-parameterize stx)
      (build-parameterize (syntax-object-srcloc stx)
                          (map expand params)
                          (map expand inits)
                          (expand-body body (make-environment))))))

(define (parse-parameterize stx)
  (let ((fail
         (lambda ()
           (raise-syntax-error stx "ill-formed parameterize form")))
        (form (syntax->list stx)))
    (unless (and form (<= 2 (length form))) (fail))
    (let ((bindings (syntax->list (cadr form))))
      (unless bindings (fail))
      (receive (params inits)
          (let f ((bindings bindings))
            (if (null? bindings)
                (values '() '())
                (let-values (((params inits) (f (cdr bindings)))
                             ((form) (syntax->list (car bindings))))
                  (unless (and form (= 2 (length form))) (fail))
                  (values (cons (car form) params)
                          (cons (cadr form) inits)))))
        (values params inits (syntax-cdr (syntax-cdr stx)))))))

;;;;;;;;;;;;;;;;
;; Quotations ;;
;;;;;;;;;;;;;;;;

(define-core-form! 'quote
  (lambda (stx)
    (build-literal (syntax-object-srcloc stx)
                   (syntax->datum (parse-quotation stx)))))

(define (parse-quotation stx)
  (let ((form (syntax->list stx)))
    (unless (and form (= 2 (length form)))
      (raise-syntax-error stx "ill-formed quotation"))
    (cadr form)))

;;;;;;;;;;;;;;;;
;; Assignment ;;
;;;;;;;;;;;;;;;;

(define-core-form! 'set!
  (lambda (stx)
    (let*-values (((srcloc) (syntax-object-srcloc stx))
                  ((id expr) (parse-assignment stx))
                  ((binding) (lookup (resolve id)))
                  ((val) (binding-value binding)))
      (case (binding-type binding)
        ((macro macro-parameter)
         (expand (transform/macro val stx #f id)))
        ((global-keyword)
         (expand (transform/global-keyword val stx #f id)))
        ((primitive global-variable)
         (raise-syntax-error stx "trying to modify imported identifier ‘~a’"
                             (identifier-name stx)))
        ((mutable-variable)
         (build-global-assignment
          srcloc (build-reference (syntax-object-srcloc id) val) (expand expr)))
        ((lexical)
         (build-assignment srcloc
                           (build-reference (syntax-object-srcloc id) val)
                           (expand expr)))
        ((#f)
         (raise-syntax-error stx "unbound variable ‘~a’" (identifier-name stx)))
        (else
         (raise-syntax-error stx "not a variable"))))))

(define (parse-assignment stx)
  (let ((form (syntax->list stx)))
    (unless (and form (= 3 (length form)) (identifier? (cadr form)))
      (raise-syntax-error stx "ill-formed assignment"))
    (values (cadr form) (caddr form))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delayed evaluations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define-core-form! 'delay
  (lambda (stx)
    (build-delay (syntax-object-srcloc stx) (expand (parse-delay stx)))))

(define-core-form! 'delay-force
  (lambda (stx)
    (build-delay-force (syntax-object-srcloc stx)
                       (expand (parse-delay-force stx)))))

(define (parse-delay stx)
  (let ((form (syntax->list stx)))
    (unless (and form (= 2 (length form)))
      (raise-syntax-error stx "ill-formed delay form"))
    (cadr form)))

(define (parse-delay-force stx)
  (let ((form (syntax->list stx)))
    (unless (and form (= 2 (length form)))
      (raise-syntax-error stx "ill-formed delay-force form"))
    (cadr form)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax-Parameterize ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define-core-form! 'syntax-parameterize
  (lambda (stx)
    (let*-values (((ids inits body) (parse-syntax-parameterize stx))
                  ((lbls) (map resolve ids)))
      (for-each
       (lambda (id lbl)
         (let* ((binding (lookup lbl))
                (type (binding-type binding)))
           (unless (or (eq? 'macro-parameter type)
                       (and (eq? 'global-keyword type)
                            (eq? 'macro-parameter
                                 (cadr (binding-value binding)))))
             (raise-syntax-error
              id "trying to parameterize the non-syntax parameter ‘~a’"
              (identifier-name id)))))
       ids lbls)
      (with-bindings lbls
          (map (lambda (init)
                 (make-transformer-binding 'define-syntax-parameter
                                           (expand-transformer init)))
               inits)
        (expand-body body (make-environment))))))

(define (parse-syntax-parameterize stx)
  (let ((fail
         (lambda ()
           (raise-syntax-error stx "ill-formed syntax-parameterize form")))
        (form (syntax->list stx)))
    (unless (and form (<= 2 (length form))) (fail))
    (let ((bindings (syntax->list (cadr form))))
      (unless bindings (fail))
      (receive (params inits)
          (let f ((bindings bindings))
            (if (null? bindings)
                (values '() '())
                (let-values (((params inits) (f (cdr bindings)))
                             ((form) (syntax->list (car bindings))))
                  (unless (and form (= 2 (length form))) (fail))
                  (values (cons (car form) params)
                          (cons (cadr form) inits)))))
        (values params inits (syntax-cdr (syntax-cdr stx)))))))
