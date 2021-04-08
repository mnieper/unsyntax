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

(define (expand-meta stx)
  (receive (expr inv-reqs)
      (parameterize ((invoke-collector (make-library-collector))
                     (visit-collector (make-library-collector)))
        (let ((expr (in-meta (expand stx))))
          (values expr (invoke-requirements))))
    (for-each (lambda (lib)
                (invoke-library! lib)
                (require-visit! lib))
              inv-reqs)
    expr))

;;;;;;;;;;;;;;;;
;; Properties ;;
;;;;;;;;;;;;;;;;

(define (expand-property id stx)
  (list (expand-meta stx) id))

(define (make-property-binding def)
  (make-binding 'property (cons (execute (car def)) (cdr def))))

(define (property-lookup id key)
  (assert (identifier? id))
  (assert (identifier? key))
  (let ((klbl (or (resolve key)
                  (raise-syntax-error key "identifier ‘~a’ unbound"
                                      (identifier-name key)))))
    (let ((ilbl (or (resolve id)
                    (raise-syntax-error id "identifier ‘~a’ unbound"
                                        (identifier-name id)))))
      (if (and (eq? klbl 'auxiliary-syntax-name) (auxiliary-syntax-label? ilbl))
          (auxiliary-syntax-name ilbl)
          (and-let* ((plbl (resolve-prop id ilbl klbl))
                     (b (lookup plbl))
                     (type (binding-type b))
                     (val (binding-value b)))
            (case type
              ((property)
               (car val))
              ((global-property)
               (and-let* ((lib (car val)))
                 (visit-library! lib))
               (caadr val))
              (else (error "compiler error"))))))))

(define (property-aware transformer)
  (lambda (stx)
    (let f ((res (transformer stx)))
      (if (procedure? res)
          (f (res property-lookup))
          res))))

(define (er-property-aware transformer inject)
  (lambda (expr rename compare)
    (let f ((res (transformer expr rename compare)))
      (if (procedure? res)
          (f (res (lambda (id key)
                    (property-lookup (inject id) (inject key)))))
          res))))

(define (ir-property-aware transformer rename)
  (lambda (expr inject compare)
    (let f ((res (transformer expr inject compare)))
      (if (procedure? res)
          (f (res (lambda (id key)
                    (property-lookup (rename id) (rename key)))))
          res))))

(define (sc-property-aware transformer)
  (lambda (exp env)
    (let f ((res (transformer exp env)))
      (if (procedure? res)
          (f (res (lambda (id-env id key-env key)
                    (property-lookup (close-syntax id id-env)
                                     (close-syntax key key-env)))))
          res))))

;;;;;;;;;;;;;;;;;;;;;;
;; Meta Definitions ;;
;;;;;;;;;;;;;;;;;;;;;;

(define (expand-meta-definition id stx n)
  (list (build (syntax-object-srcloc stx)
               (arguments->vector ',id ',n (lambda () ,(expand-meta stx))))
        id))

(define (eval-meta stx)
  (execute (expand-meta stx)))

(define (make-meta-variable-binding plbl i)
  (make-meta-binding 'meta-variable (list plbl i)))

(define (meta-lookup id lbl)
  (define b (lookup lbl))
  (unless b
    (raise-syntax-error id "circular reference of meta variabie ‘~a’"
			(identifier-name id)))
  (let ((val (binding-value b)))
    (case (binding-type b)
      ((property)
       (car val))
      ((global-property)
       (and-let* ((lib (car val)))
         (visit-library! lib))
       (caadr val)))))

(define (meta-unbox id plbl i)
  (define vals (meta-lookup id plbl))
  (vector-ref vals i))

(define (meta-set-box! id plbl i obj)
  (define vals (meta-lookup id plbl))
  (vector-set! vals i obj))

;;;;;;;;;;;;;;;;;;
;; Transformers ;;
;;;;;;;;;;;;;;;;;;

(define (expand-transformer id stx)
  (list (expand-meta stx) id))

(define (make-transformer-binding type def)
  (let ((transformer (execute (car def))))
    (make-binding (case type
                    ((define-syntax) 'macro)
                    ((define-syntax-parameter) 'macro-parameter))
                  (cons transformer (cdr def)))))

(define transform/global-keyword
  (case-lambda
    ((val stx env)
     (transform/global-keyword val stx env #f))
    ((val stx env id)
     (and-let* ((lib (car val)))
       (visit-library! lib))
     (transform/macro (caddr val) stx env id))))

(define transform/macro
  (case-lambda
    ((def stx env)
     (transform/macro def stx env #f))
    ((def stx env id)
     (let* ((maybe-variable-transformer (car def)) (transformer-stx (cadr def))
            (transformer
             (cond
              ((variable-transformer? maybe-variable-transformer)
               (variable-transformer-procedure maybe-variable-transformer))
              (id
               (raise-syntax-error id "not a variable transformer ‘~s’"
                                   (identifier-name id)))
              (else
               maybe-variable-transformer))))
       (cond
        ((procedure? transformer)
         (apply-transformer (property-aware transformer)
                            stx (add-mark (anti-mark) stx) env))
        ((er-macro-transformer? transformer)
         (er-transform transformer stx (add-mark (anti-mark) stx)
                       transformer-stx env))
        ((ir-macro-transformer? transformer)
         (ir-transform transformer stx (add-mark (anti-mark) stx)
                       transformer-stx env))
        ((sc-macro-transformer? transformer)
         (sc-transform transformer stx transformer-stx))
        (else
         (raise-syntax-error transformer-stx "‘~a’ is not a transformer"
                             (identifier-name transformer-stx))))))))

(define (apply-transformer transform stx e env)
  (let* ((loc (syntax-object-srcloc stx))
         (wrap (lambda (e) (datum->syntax #f e loc))))
    (wrap (build-output stx
                        (transform e)
                        (make-mark)
                        env))))

(define (apply-transformer/closure transform stx e k env)
  (let* ((loc (syntax-object-srcloc stx))
         (wrap (lambda (e) (datum->syntax #f e loc))))
    (wrap (build-output/closure stx k
                                (transform e)
                                (make-mark)
                                env))))

(define (build-output stx e m env)
  (let f ((e e))
    (cond
     ((pair? e)
      (cons (f (car e)) (f (cdr e))))
     ((vector? e)
      (vector-map f e))
     ((symbol? e)
      (raise-syntax-error
       stx "encountered raw symbol ‘~a’ in output of macro" e))
     ((syntax-object? e)
      (receive (m* s*)
          (let ((m* (syntax-object-marks e))
                (s* (syntax-object-substs e)))
            (if (and (pair? m*) (anti-mark? (car m*)))
                (values (cdr m*) (cdr s*))
                (values (cons m m*)
                        (if env
                            (cons* env (shift) s*)
                            (cons (shift) s*)))))
        (make-syntax-object (syntax-object-expr e)
                            m*
                            s*
                            (syntax-object-srcloc e))))
     (else
      ;; TODO: Check whether this is an allowed datum.
      e))))

(define (build-output/closure stx k e m env)
  (let ((table (make-hash-table eq-comparator)))
    (let f ((e e))
      (cond
       ((hash-table-ref/default table e #f))
       ((pair? e)
        (let ((v (cons #f #f)))
          (hash-table-set! table e v)
          (set-car! v (f (car e)))
          (set-cdr! v (f (cdr e)))
          v))
       ((vector? e)
        (let ((v (make-vector (vector-length e))))
          (hash-table-set! table e v)
          (do ((i 0 (+ i 1)))
              ((= i (vector-length v)) v)
            (vector-set! v i (f (vector-ref e i))))))
       ((symbol? e)
        (f (datum->syntax k e)))
       ((syntax-object? e)
        (receive (m* s*)
            (let ((m* (syntax-object-marks e))
                  (s* (syntax-object-substs e)))
              (if (and (pair? m*) (anti-mark? (car m*)))
                  (values (cdr m*) (cdr s*))
                  (values (cons m m*)
                          (if env
                              (cons* env (shift) s*)
                              (cons (shift) s*)))))
          (make-syntax-object (syntax-object-expr e)
                              m*
                              s*
                              (syntax-object-srcloc e))))
       (else
        ;; TODO: Check whether this is an allowed datum.
        e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ER macro transformer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (er-transform transformer stx e k env)
  (let* ((i (if (identifier? e) e (syntax-car e)))
         (inject (lambda (id)
                   (if (identifier? id)
                       id
                       (datum->syntax i id))))
         (transform
          (er-property-aware (er-macro-transformer-procedure transformer)
                             inject)))
    (apply-transformer/closure
     (lambda (stx)
       (transform
        (syntax->sexpr e)
        (lambda (exp)
          (datum->syntax k exp))
        (lambda (id1 id2)
          (free-identifier=? (inject id1) (inject id2)))))
     stx e i env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IR macro transformer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ir-transform transformer stx e k env)
  (let* ((rename (lambda (id)
                   (if (identifier? id)
                       id
                       (datum->syntax k id))))
         (transform
          (ir-property-aware (ir-macro-transformer-procedure transformer)
                             rename)))
    (apply-transformer/closure
     (lambda (stx)
       (let ((i (if (identifier? e) e (syntax-car e))))
         (transform (syntax->sexpr e)
                    (lambda (exp)
                      (datum->syntax i exp))
                    (lambda (id1 id2)
                      (free-identifier=? (rename id1) (rename id2))))))
     stx e k env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SC macro transformer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sc-transform transformer stx k)
  (let ((transform (sc-macro-transformer-procedure transformer))
        (i (if (identifier? stx) stx (syntax-car stx))))
    (sc-build-output stx (transform (syntax->sexpr stx i) i)
                     (add-mark (make-mark) k))))

(define (sc-build-output stx e k)
  (let* ((loc (syntax-object-srcloc stx))
	 (wrap (lambda (e) (datum->syntax #f e loc)))
         (table (make-hash-table eq-comparator)))
    (wrap
     (let g ((e e) (k k) (contexts '()))
       (let f ((e e))
	 (cond
          ;; FIXME: The same pair or vector may appear in different
          ;; syntactic closures.  For the moment, disable gracefully
          ;; handling of cyclic data structures.
          #;
          ((hash-table-ref/default table e #f))
	  ((pair? e)
           (let ((v (cons #f #f)))
             (hash-table-set! table e v)
             (set-car! v (f (car e)))
             (set-cdr! v (f (cdr e)))
             v))
	  ((vector? e)
           (let ((v (make-vector (vector-length e))))
             (hash-table-set! table e v)
             (do ((i 0 (+ i 1)))
                 ((= i (vector-length v)) v)
               (vector-set! v i (f (vector-ref e i))))))
	  ((symbol? e)
	   (cond
	    ((assoc (datum->syntax #f e) contexts bound-identifier=?)
	     => (lambda (pair)
		  (datum->syntax (cdr pair) (car pair))))
	    (else
             (datum->syntax k e))))
	  ((syntax-object? e)
	   (cond
	    ((and (identifier? e) (assoc e contexts bound-identifier=?))
	     => (lambda (pair)
		  (datum->syntax (cdr pair) (car pair))))
	    (else (datum->syntax k e))))
	  ((syntactic-closure? e)
	   (let ((i (syntactic-closure-environment e)))
	     (g (syntactic-closure-form e)
		i
		(map (lambda (id)
		       (let ((id (if (identifier? id)
                                     id
                                     (datum->syntax #f id))))
			 (cond
			  ((assoc id contexts bound-identifier=?))
			  (else (cons id k)))))
		     (syntactic-closure-free-names e)))))
	  ((capture-syntactic-environment? e)
	   (datum->syntax
	    k
	    (capture-syntactic-environment
	     (lambda (env)
	       (wrap (f ((capture-syntactic-environment-procedure e) env)))))
	    loc))
	  (else
	   e)))))))
