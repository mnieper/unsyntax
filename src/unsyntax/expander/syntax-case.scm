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

(define (pattern-constant? datum)
  (or (boolean? datum)
      (bytevector? datum)
      (char? datum)
      (null? datum)
      (number? datum)
      (string? datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special Identifiers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ellipsis-identifier? id)
  (cond
   ((lookup (resolve (ellipsis-identifier id)))
    => (lambda (b)
         (bound-identifier=? b id)))
   (else
    (free-identifier=? (core-syntax '...) id))))

;;;;;;;;;;;;;;;;;
;; Syntax Case ;;
;;;;;;;;;;;;;;;;;

(define current-literal? (make-parameter #f))
(define current-ellipsis? (make-parameter ellipsis-identifier?))

(define (literal? id)
  ((current-literal?) id))

(define (underscore? id)
  (free-identifier=? (core-syntax '_) id))

(define (ellipsis? id)
  ((current-ellipsis?) id))

(define (make-pvar-binding pvar)
  (make-binding 'syntax pvar))

(define-core-form! 'syntax-case
  (lambda (stx)
    (let*-values (((expr lits clauses) (parse-syntax-case stx))
                  ((e) (generate-variable "s"))
                  ((loc) (syntax-object-srcloc stx)))
      (build loc
        (let ((e ,(expand expr)))
          ,(parameterize ((current-literal?
                           (lambda (id)
                             (member id lits free-identifier=?))))
             (fold-right (lambda (clause next)
                           (let ((f (generate-variable "f")))
                             (build loc
                               (let ((f (lambda () ,next)))
                                 ,(gen-clause loc e clause
                                              (lambda ()
                                                (build loc
                                                  (f))))))))
                         (build loc
                           (raise-syntax-error e '"syntax error"))
                         clauses)))))))

(define (gen-clause loc e clause f)
  (let*-values (((pattern fender output) (parse-syntax-case-clause clause))
                ((matcher pvars) (gen-matcher loc e pattern '() f))
                ((env) (make-environment))
                ((lbls) (map genlbl pvars)))
    (for-each (lambda (pvar lbl)
                (environment-set! env (car pvar) lbl))
              pvars lbls)
    (matcher (lambda ()
               (with-frame lbls
                   (map (lambda (pvar)
                          (make-pvar-binding (cdr pvar)))
                        pvars)
                 (let ((expanded-output (expand (add-substs env output))))
                   (if fender
                       (build loc
                         (if ,(expand (add-substs env fender))
                             ,expanded-output
                             ,(f)))
                       expanded-output)))))))

(define (gen-matcher loc e pat pvars f)
  (cond
   ((syntax-pair? pat)
    (cond
     ;; (<pattern> <ellipsis> . <pattern>)
     ((and-let* ((p (syntax-cdr pat))
                 ((syntax-pair? p))
                 (id (syntax-car p))
                 ((identifier? id))
                 ((ellipsis? id)))
        (syntax-cdr p))
      => (lambda (tail-pat)
           (let ((l (syntax-length+ tail-pat))
                 (h (generate-variable "s"))
                 (t (generate-variable "s"))
                 (n (generate-variable "s")))
             (let*-values (((head-matcher pvars)
                            (gen-map loc h (syntax-car pat) pvars f))
                           ((tail-matcher pvars)
                            (gen-matcher* loc t tail-pat pvars f)))
               (values (lambda (k)
                         (build loc
                           (let ((n (syntax-length+ e)))
                             (if n
                                 (if (>= n ',l)
                                     (receive (h t)
                                         (syntax-split-at e (- n ',l))
                                       ,(head-matcher (lambda ()
                                                        (tail-matcher k))))
                                     ,(f))
                                 ,(f)))))
                       pvars)))))
     (else
      ;; (<pattern> . <pattern>)
      (let ((e1 (generate-variable "s"))
            (e2 (generate-variable "s")))
        (let*-values
            (((car-matcher pvars)
              (gen-matcher loc e1 (syntax-car pat) pvars f))
             ((cdr-matcher pvars)
              (gen-matcher loc e2 (syntax-cdr pat) pvars f)))
          (values (lambda (k)
                    (build loc
                      (if (syntax-pair? e)
                          (let ((e1 (syntax-car e))
                                (e2 (syntax-cdr e)))
                            ,(car-matcher (lambda () (cdr-matcher k))))
                          ,(f))))
                  pvars))))))
   ;; #(<pattern> ...)
   ((syntax-vector? pat)
    (let*-values (((g) (generate-variable "s"))
                  ((matcher pvars)
                   (gen-matcher loc g (syntax-vector->list pat) pvars f)))
      (values (lambda (k)
                (build loc
                  (if (syntax-vector? e)
                      (let ((g (syntax-vector->list e)))
                        ,(matcher k))
                      ,(f))))
              pvars)))
   ((identifier? pat)
    (cond
     ;; <literal>
     ((literal? pat)
      (values (lambda (k)
                (build loc
                  (if (free-identifier=? ',pat e)
                      ,(k)
                      ,(f))))
              pvars))
     ;; <underscore>
     ((underscore? pat)
      (values (lambda (k) (k)) pvars))
     ;; <ellipsis>
     ((ellipsis? pat)
      (raise-syntax-error pat "misplaced ellipsis in syntax pattern"))
     ;; <pattern variable>
     (else
      (values (lambda (k) (k))
              (alist-cons pat (list e 0) pvars)))))
   (else
    (let ((datum (syntax-object-expr pat)))
      (unless (pattern-constant? datum)
        (raise-syntax-error pat "invalid pattern"))
      ;; <constant>
      (values (lambda (k)
                (build loc
                  (if (equal? ',datum (syntax->datum e))
                      ,(k)
                      ,(f))))
              pvars)))))

(define (gen-matcher* loc e pat* pvars f)
  (cond
   ((syntax-null? pat*)
    (values (lambda (k)
              (build loc (if (syntax-null? e) ,(k) ,(f))))
            pvars))
   ((syntax-pair? pat*)
    (let ((e1 (generate-variable "s"))
          (e2 (generate-variable "s")))
      (let*-values (((car-matcher pvars)
                     (gen-matcher loc e1 (syntax-car pat*) pvars f))
                    ((cdr-matcher pvars)
                     (gen-matcher* loc e2 (syntax-cdr pat*) pvars f)))
        (values (lambda (k)
                  (build loc
                    (let ((e1 (syntax-car e))
                          (e2 (syntax-cdr e)))
                      ,(car-matcher (lambda () (cdr-matcher k))))))
                pvars))))
   (else
    (gen-matcher loc e pat* pvars f))))

(define (gen-map loc e pat pvars f)
  (let ((g (generate-variable "s"))
        (h (generate-variable "s"))
        (l (generate-variable "s")))
    (receive (matcher inner-pvars) (gen-matcher loc g pat '() f)
      (let ((g* (map (lambda (v) (generate-variable "p")) inner-pvars)))
        (values (lambda (k)
                  (build loc
                    (let l ((h (reverse e))
                            (g* ,(map (lambda (g)
                                        (build loc '()))
                                      g*))
                            ...)
                      (if (null? h)
                          ,(k)
                          (let ((g (car h)))
                            ,(matcher (lambda ()
                                        (build loc
                                          (l (cdr h)
                                             ,(map (lambda (pvar g)
                                                     (let ((i (cadr pvar)))
                                                       (build loc
                                                         (cons i g))))
                                                   inner-pvars g*)
                                             ...)))))))))
                (fold (lambda (g pvar pvars)
                        (alist-cons (car pvar) (list g (+ 1 (caddr pvar)))
                                    pvars))
                      pvars g* inner-pvars))))))

(define (parse-syntax-case stx)
  (let ((fail
         (lambda ()
           (raise-syntax-error stx "ill-formed syntax-case form")))
        (form (syntax->list stx)))
    (unless (and form (<= 3 (length form))) (fail))
    (let ((lits (syntax->list (caddr form))))
      (unless lits (fail))
      (for-each (lambda (lit)
                  (unless (identifier? lit)
                    (raise-syntax-error stx "invalid literal")))
                lits)
      (values (cadr form) lits (cdddr form)))))

(define (parse-syntax-case-clause stx)
  (let ((form (syntax->list stx)))
    (unless (and form (<= 2 (length form) 3))
      (raise-syntax-error stx "ill-formed syntax-case clause"))
    (if (= 2 (length form))
        (values (car form) #f (cadr form))
        (values (car form) (cadr form) (caddr form)))))

;;;;;;;;;;;;
;; Syntax ;;
;;;;;;;;;;;;

(define (pattern-variable id)
  (let ((b (lookup (resolve id))))
    (and (eq? 'syntax (binding-type b))
         (binding-value b))))

(define (syntax-transformer lvl)
  (lambda (stx)
    (let*-values (((tmpl) (parse-syntax stx))
                  ((out env var?)
                   (gen-template tmpl '() lvl #f)))
      out)))

(define-core-form! 'syntax (syntax-transformer #f))
(define-core-form! 'quasisyntax (syntax-transformer 0))

(define (gen-template tmpl envs lvl tail?)
  (let ((loc (syntax-object-srcloc tmpl)))
    (cond
     ((syntax-pair? tmpl)
      (cond
       ;; Circular list in source?
       ((syntax-circular-list? tmpl)
        (raise-syntax-error tmpl "circular list in syntax template"))
       ;; (unsyntax <template>)
       ((and-let* ((lvl)
                   (id (syntax-car tmpl))
                   ((identifier? id))
                   ((free-identifier=? (core-syntax 'unsyntax) id))))
        (let ((form (syntax->list tmpl)))
          (unless (and form (= 2 (length form)))
            (raise-syntax-error tmpl "ill-formed unsyntax form"))
          (if (zero? lvl)
              (values (expand (cadr form)) envs #t)
              (receive (out envs var?)
                  (gen-template (cadr form) envs (- lvl 1) tail?)
                (if var?
                    (values (build loc
                              (list ',(car form) ,out))
                            envs
                            #t)
                    (values (build loc ',tmpl) envs #f))))))
       ;; (quasisyntax <template>)
       ((and-let* ((lvl)
                   (id (syntax-car tmpl))
                   ((identifier? id))
                   ((free-identifier=? (core-syntax 'quasisyntax) id)))
          (let ((form (syntax->list tmpl)))
            (unless (and form (= 2 (length form)))
              (raise-syntax-error tmpl "ill-formed quasisyntax form"))
            (receive (out envs var?)
                (gen-template (cadr form) envs (+ lvl 1) tail?)
              (if var?
                  (values (build loc (list ',(car form) ,out)) envs #t)
                  (values (build loc ',tmpl) envs #f))))))
       ;; (<ellipsis> <template>)
       ((and-let* ((id (syntax-car tmpl))
		   ((identifier? id))
		   ((ellipsis? id))
		   (p (syntax-cdr tmpl))
		   ((syntax-pair? p))
		   ((syntax-null? (syntax-cdr p))))
	  (syntax-car p))
	=> (lambda (tmpl)
	     (receive (out envs var?)
		 (parameterize ((current-ellipsis? (lambda (id) #f)))
		   (gen-template tmpl envs lvl tail?))
	       (values out envs #t))))
       ;; (<template> ... . <template>)
       ((and-let* ((p (syntax-cdr tmpl))
                   ((syntax-pair? p))
                   (id (syntax-car p))
                   ((identifier? id))
                   ((ellipsis? id)))
          (syntax-cdr p))
        => (lambda (t)
             (let ((h (syntax-car tmpl))
                   (s (generate-variable "s")))
               (let*-values (((out* envs var?)
                              (gen-template t envs lvl #t))
                             ((out envs var?)
                              (gen-template h (cons '() envs) lvl #f))
			     ((vars) (car envs)))
                 (when (null? vars)
                   (raise-syntax-error
                    h "too many ellipses following syntax template"))
                 (values (build loc
                           (fold-right (lambda (vars ... s)
                                         (cons ,out s))
                                       ,out*
                                       ,(map (lambda (var)
					       (build loc var))
					     vars)
				       ...))
                         (cdr envs)
                         #t)))))
       ;; (<template> . <element>)
       (else
        (let*-values (((out1 envs var1?)
                       (gen-template (syntax-car tmpl) envs lvl #f))
                      ((out2 envs var2?)
                       (gen-template (syntax-cdr tmpl) envs lvl #t)))
          (if (or var1? var2?)
              (values (build loc (cons ,out1 ,out2)) envs #t)
              (values (build loc ',tmpl) envs #f))))))
     ;; #(<element> ...)
     ((syntax-vector? tmpl)
      (receive (out envs var?)
	  (gen-template (datum->syntax #f (syntax-vector->list tmpl) loc)
			envs lvl #f)
	(if var?
	    (values (build loc (list->vector (syntax->list ,out))) envs #t)
	    (values (build loc ',tmpl) envs #f))))
     ;; <identifier>
     ((identifier? tmpl)
      (cond
       ((pattern-variable tmpl)
        => (lambda (val)
             (values (build loc ,(car val))
                     (update-envs tmpl (car val) (cadr val) envs)
                     #t)))
       (else
        (values (build loc ',tmpl)
                envs
                #f))))
     (else
      (let ((datum (syntax-object-expr tmpl)))
        (unless (pattern-constant? datum)
          (raise-syntax-error tmpl "invalid syntax template"))
        (values (build loc
                  ',(if (and tail? (null? datum)) '() tmpl))
                envs
                #f))))))

(define (update-envs id x lvl envs)
  (let f ((lvl lvl) (envs envs))
    (cond
     ((zero? lvl)
      envs)
     ((null? envs)
      (raise-syntax-error id "too few ellipses following syntax template"))
     (else
      (let ((outer-envs (f (- lvl 1) (cdr envs))))
	(cond ((member x (car envs) variable=?) envs)
              (else (alist-cons x (car envs)
                                outer-envs))))))))

(define (parse-syntax stx)
  (let ((form (syntax->list stx)))
    (unless (and form (= (length form) 2))
      (raise-syntax-error stx "ill-formed syntax form"))
    (cadr form)))
