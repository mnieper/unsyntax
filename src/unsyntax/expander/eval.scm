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

;;;;;;;;;;;;;;;;;;
;; Environments ;;
;;;;;;;;;;;;;;;;;;

(define (environment . import-sets)
  (let ((env (make-environment)))
    (%environment-import env import-sets)
    env))

(define (mutable-environment . import-sets)
  (let ((env (make-mutable-environment)))
    (environment-set! env (datum->syntax #f 'import) 'import)
    (%environment-import env import-sets)
    env))

(define (%environment-import env import-sets)
  (for-each (lambda (import-set)
              (environment-import env (datum->syntax #f import-set)))
            import-sets))

(define (environment-define! env name val)
  (unless (environment-mutable? env)
    (raise-error 'environment-define
                 "trying to define ‘~a’ in immutable environment" name))
  (mutable-set! env (datum->syntax #f name) val))

(define (mutable-ref/props env id)
  (or (environment-ref/props env id)
      (and (symbol? (identifier-name id))
           (let* ((var (genvar id))
                 (lbl (genlbl id))
                 (l/p (make-label/props lbl '())))
             (environment-set!/props env id l/p)
             (bind-global! lbl (make-binding 'mutable-variable var))
             (set-global! var (box (if #f #f)))
             l/p))))

(define (mutable-ref env id)
  (and-let* ((l/p (mutable-ref/props env id)))
    (label/props-label l/p)))

(define (mutable-set! env id val)
  (let* ((lbl (mutable-ref env id))
         (b (lookup lbl)))
    (if (eq? 'mutable-variable (binding-type b))
        (set-box! (ref-global (binding-value b)) val)
        (let ((var (genvar id)))
          (bind-global! lbl (make-binding 'mutable-variable var))
          (set-global! var (box val))))))

;;;;;;;;;;;;;;;;
;; Evaluation ;;
;;;;;;;;;;;;;;;;

(define (eval expr env)
  (eval-syntax (datum->syntax #f expr) env))

(define (eval-syntax stx env)
  (let ((wrapped (add-substs env stx)))
    (if (environment-mutable? env)
        (eval-repl wrapped env)
        (eval-expression wrapped))))

(define (eval-expression stx)
  (receive (expr invreqs) (expand-expression stx)
    (for-each invoke-library! invreqs)
    (execute expr)))

(define (expand-expression stx)
  (parameterize ((invoke-collector (make-library-collector)))
    (let ((expr (expand stx)))
      (values expr (invoke-requirements)))))

(define (eval-repl stx env)
  (parameterize ((current-global-resolver
                  (lambda (id) (mutable-ref/props env id))))
    (let f ((body (list stx)) (vals (list (if #f #f))))
      (if (null? body)
          (apply values vals)
          (receive (stx type val) (syntax-type (car body) env)
            (case type
              ((alias)
               (let*-values (((id1 id2) (parse-alias stx))
                             ((lbl) (resolve id2)))
                 (environment-set! env id1 lbl)
                 (f (cdr body) (list (if #f #f)))))
              ((begin)
               (f (append (parse-begin stx #f) (cdr body)) vals))
              ((define-auxiliary-syntax)
               (receive (id sym) (parse-define-auxiliary-syntax stx)
                 (environment-set! env id (auxiliary-syntax-label sym)))
               (f (cdr body) (list (if #f #f))))
              ((define-property)
               (expand-define-property stx
                                       (lambda (id l/p)
                                         (environment-set!/props env id l/p)))
               (f (cdr body) (list (if #f #f))))
              ((define-record-type)
	       (define-record-type! stx env)
	       (f (cdr body) (list (if #f #f))))
              ((define-values)
               (define-values! stx env)
               (f (cdr body) (list (if #f #f))))
              ((define-syntax define-syntax-parameter)
               (expand-define-syntax type stx (lambda (id lbl)
                                                (environment-set! env id lbl)))
               (f (cdr body) (list (if #f #f))))
              ((import)
               (environment-import* env (parse-import-declaration stx))
               (f (cdr body) (list (if #f #f))))
              ((let-syntax letrec-syntax)
               (f (append (expand-let-syntax* type stx) (cdr body)) vals))
              ((with-ellipsis)
               (f (append (expand-with-ellipsis* stx) (cdr body)) vals))
              (else
               (receive vals (eval-expression stx)
                 (f (cdr body) vals)))))))))

(define (define-record-type! stx env)
  (let*-values
      (((loc) (syntax-object-srcloc stx))
       ((rtd cname pred cfields names accs muts)
	(parse-define-record-type stx))
       ((indices) (get-field-indices names cfields))
       ((cvar) (genvar cname))
       ((pvar) (genvar pred))
       ((avars) (map genvar accs))
       ((mvars) (map (lambda (mut) (and mut (genvar mut))) muts))
       ((cref) (lambda ()
		 (build-reference (syntax-object-srcloc cname)
				  cvar)))
       ((pref) (lambda ()
		 (build-reference (syntax-object-srcloc pred)
				  pvar)))
       ((arefs) (lambda ()
		  (map (lambda (acc avar)
			 (build-reference (syntax-object-srcloc acc)
					  avar))
		       accs avars)))
       ((mrefs) (lambda ()
		  (map (lambda (mut mvar)
			 (and mut
			      (build-reference (syntax-object-srcloc mut)
					       mvar)))
		       muts mvars)))
       ((cval pval avals mvals)
	(execute
	 (build-body
	  loc
	  (list
	   (build-define-record-type
	    loc (identifier-name rtd)
	    (cref) (pref)
	    indices (map identifier-name names)
	    (arefs)
	    (mrefs)))
	  (build-primitive-call
	   loc 'values
	   (list (cref) (pref)
		 (build-primitive-call loc 'list (arefs))
		 (build-primitive-call
		  loc 'list
		  (map (lambda (mref)
			 (or mref (build-literal loc #f)))
		       (mrefs)))))))))
    (mutable-set! env cname cval)
    (mutable-set! env pred pval)
    (for-each (lambda (acc aval)
		(mutable-set! env acc aval))
	      accs avals)
    (for-each (lambda (mut mval)
		(when mut
		  (mutable-set! env mut mval)))
	      muts mvals)
    (let ((rlbl (genlbl rtd)))
      (environment-set! env rtd rlbl)
      (bind! rlbl (make-binding 'record-type-descriptor #f)))))

(define (define-values! stx env)
  (let*-values (((formals init) (parse-define-values stx))
                ((ids variadic?) (parse-formals formals))
                (vals (eval-expression init))
                ((expected-arg-num)
                 (if variadic? (- (length ids) 1) (length ids)))
                ((actual-arg-num) (length vals)))
    (when (< actual-arg-num expected-arg-num)
      (syntax-violation #f "not enough values" stx init))
    (when (and (not variadic?) (< expected-arg-num actual-arg-num))
      (syntax-violation #f "too many values" stx init))
    (let f ((ids ids) (vals vals))
      (unless (null? ids)
        (let ((id (car ids)))
          (cond ((and variadic? (null? (cdr ids)))
                 (mutable-set! env id vals))
                (else
                 (mutable-set! env id (car vals))
                 (f (cdr ids) (cdr vals)))))))))

(define (parse-import-declaration stx)
  (let ((form (syntax->list stx)))
    (unless (and (pair? form)
		 (identifier? (car form))
		 (eq? 'import (identifier-name (car form))))
      (raise-syntax-error stx "ill-formed import declaration"))
    (cdr form)))
