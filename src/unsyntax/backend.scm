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

;;;;;;;;;;;;;;
;; Builders ;;
;;;;;;;;;;;;;;

(define (constant? e)
  (or (boolean? e)
      (bytevector? e)
      (char? e)
      (number? e)
      (string? e)
      (vector? e)))

(define (build-assignment loc ref expr)
  `(set! ,ref ,expr))

(define (build-begin loc exprs)
  (cond
   ((null? exprs)
    `(if #f #f))
   ((null? (cdr exprs))
    (car exprs))
   (else
    `(begin ,@exprs))))

(define (build-body loc defs expr)
  (if (null? defs)
      expr
      `(let* () ,@defs ,expr)))

(define (build-call loc operator operands)
  `(,operator ,@operands))

(define (build-case-lambda loc formals-list body-list)
  (if (and (pair? formals-list)
           (null? (cdr formals-list)))
      `(lambda ,(car formals-list) ,(car body-list))
      `(case-lambda ,@(map (lambda (formals body)
                             `(,formals ,body))
                           formals-list body-list))))

(define build-conditional
  (case-lambda
    ((loc test consequent)
     `(if ,test ,consequent))
    ((loc test consequent alternate)
     `(if ,test ,consequent ,alternate))))

(define (build-define-record-type loc name cref pref indices names arefs mrefs)
  (let* ((tmps (map (lambda (name) (gensym "r")) names))
         (tmpvec (list->vector tmps)))
    `(define-record-type ,name
       (,cref ,@(map (lambda (i) (vector-ref tmpvec i)) indices))
       ,pref
       ,@(map (lambda (tmp name acc mut)
                `((,tmp ,name) ,acc ,@(if mut `(,mut) '())))
              tmps names arefs mrefs))))

(define (build-define-values loc formals init)
  (if (and (pair? formals) (null? (cdr formals)))
      `(define ,(car formals) ,init)
      `(define-values ,formals ,init)))

(define (build-delay loc expr)
  `(delay ,expr))

(define (build-delay-force loc expr)
  `(delay-force ,expr))

(define (build-formals loc refs variadic?)
  (if variadic?
      (apply cons* refs)
      refs))

(define (build-let-values loc formals-list inits expr)
  `(let-values ,(map (lambda (formals init)
                       `(,formals ,init))
                     formals-list inits)
     ,expr))

(define (build-letrec loc refs inits expr)
  `(letrec ,(map (lambda (ref init)
                   `(,ref ,init))
                 refs inits)
     ,expr))

(define (build-literal loc val)
  (if (constant? val)
      val
      `',val))

(define (build-parameterize loc params inits body)
  `(parameterize ,(map (lambda (param init)
                         `(,param ,init))
                       params inits)
     ,body))

(define (build-primitive loc prim)
  prim)

(define (build-reference loc var)
  var)

;;;;;;;;;;;;;;
;; Compiler ;;
;;;;;;;;;;;;;;

(define (extract-literals* e*)
  (let f* ((e* e*) (c '()))
    (letrec
        ((f (lambda (e c)
              (cond
               ((pair? e)
                (case (car e)
                  ((begin)
                   (receive (e* c) (f* (cdr e) c)
                     (values `(begin ,@e*) c)))
                  ((case-lambda)
                   (receive (clauses c)
                       (let g ((clauses (cdr e)) (c c))
                         (if (null? clauses)
                             (values '() c)
                             (let*-values (((clause) (car clauses))
                                           ((body c) (f (cadr clause) c))
                                           ((clauses c) (g (cdr clauses) c)))
                               (values (cons `(,(car clause) ,body) clauses)
                                       c))))
                     (values `(case-lambda ,@clauses) c)))
                  ((define)
                   (receive (init c) (f (caddr e) c)
                     (values `(define ,(cadr e) ,init) c)))
                  ((define)
                   (receive (init c) (f (caddr e) c)
                     (values `(define-values ,(cadr e) ,init) c)))
                  ((if)
                   (receive (e* c) (f* (cdr e) c)
                     (values `(if ,@e*) c)))
                  ((lambda)
                   (receive (expr c) (f (caddr e) c)
                     (values `(lambda ,(cadr e) ,expr) c)))
                  ((let)
                   (let*-values (((inits c) (f* (map cadr (cadr e)) c))
                                 ((expr c) (f (caddr e) c)))
                     (values `(let ,(map list (map car (cadr e)) inits) ,expr)
                             c)))
                  ((let*)
                   (receive (e* c) (f* (cddr e) c)
                     (values `(let* () ,@e*) c)))
                  ((quote)
                   (if (syntax-object? (cadr e))
                       (let ((v (generate-variable "c")))
                         (values v (alist-cons v (cadr e) c)))
                       (values e c)))
                  (else
                   (f* e c))))
               (else
                (if (or (pair? e) (vector? e))
                    (let ((v (generate-variable "c")))
                      (values v (alist-cons v e c)))
                    (values e c)))))))
      (if (null? e*)
          (values '() c)
          (let*-values (((e c) (f (car e*) c))
                        ((e* c) (f* (cdr e*) c)))
            (values (cons e e*) c))))))

(define (extract-literals e)
  (receive (e* c) (extract-literals* (list e))
    (values (car e*) c)))

(define (compile* exprs)
  (receive (exprs c) (extract-literals* exprs)
    (append (build-defs c) exprs)))

(define (build-defs c)
  (let* ((env-table (make-hash-table eq-comparator))
         (c
          (map (lambda (entry)
                 `(define ,(car entry)
                    ,(compile-constant (cdr entry) env-table)))
               c)))
    (hash-table-fold (lambda (env var+alist defs)
                       (if var+alist
                           (cons `(define ,(car var+alist)
                                    (environment ,@(cadr var+alist)))
                                 defs)
                           defs))
                     c env-table)))

(define (compile-constant e env-table)
  (let ((seen (make-hash-table eq-comparator))
        (intern-env!
         (lambda (env)
           (if (shift? env)
               env
               (and-let*
                   ((res
                     (hash-table-intern!
                      env-table
                      env
                      (lambda ()
                        (let ((alist (environment->alist env lookup)))
                          (if (null? alist)
                              #f
                              (list (generate-variable "e")
                                    alist)))))))
             (car res))))))
    (receive (out simple?)
        (let f ((e e))
          (letrec ((f* (lambda (e*)
                         (if (null? e*)
                             (values '() #t)
                             (let-values (((out simple?) (f (car e*)))
                                          ((out* simple*?) (f* (cdr e*))))
                               (values (cons out out*)
                                       (and simple? simple*?)))))))
            (cond
             ((hash-table-ref/default seen e #f)
              (values #f #t))
             ((list? e)
              (hash-table-set! seen e #t)
              (receive (out* simple?) (f* e)
                (values `(list ,@out*) simple?)))
             ((pair? e)
              (hash-table-set! seen e #t)
              (receive (out* simple?) (f* (list (car e) (cdr e)))
                (values `(cons ,(car out*) ,(cadr out*)) simple?)))
             ((vector? e)
              (hash-table-set! seen e #t)
              (receive (out* simple?) (f* (vector->list e))
                (values `(vector ,@out*) simple?)))
             ((syntax-object? e)
              (let*-values (((expr) (syntax-object-expr e))
                            ((out simple?) (f expr)))
                (values
                 `(syntax-object ,(if simple? (maybe-quote expr) out)
                                 ,(syntax-object-marks e)
                                 ,(filter-map intern-env!
                                              (syntax-object-substs e)))
                 #f)))
             (else
              (values (maybe-quote e) #t)))))
      (if simple? (maybe-quote e) out))))

(define (self-evaluating? e)
  (or (boolean? e)
      (bytevector? e)
      (char? e)
      (number? e)
      (string? e)
      (vector? e)))

(define (maybe-quote e)
  (if (self-evaluating? e)
      e
      `(quote ,e)))

;;;;;;;;;;;;;;
;; Executer ;;
;;;;;;;;;;;;;;

(define (execute expr)
  ;; TODO: extract-literals shall also make a list of global variables
  ;; so that we don't have to provide all.
  (let-values (((vars vals) (hash-table-entries (current-locations)))
               ((expr c) (extract-literals expr)))
    (apply (apply (eval `(lambda ,vars
                           (lambda ,(map car c)
                             ,expr)))
                  vals)
           (map cdr c))))
