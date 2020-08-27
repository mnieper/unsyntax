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

;;; FIXME: Clean up the code below.

;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary syntax ;;
;;;;;;;;;;;;;;;;;;;;;;

(define-syntax => (auxiliary-syntax '=>))
(define-syntax else (auxiliary-syntax 'else))
(define-syntax unquote (auxiliary-syntax 'unquote))
(define-syntax unquote-splicing (auxiliary-syntax 'unquote-splicing))

;;;;;;;;;;;;;;;;;;;;
;; Derived syntax ;;
;;;;;;;;;;;;;;;;;;;;

(define-syntax case
  (lambda (stx)
    (syntax-case stx ()
      ((_ e c c* ...)
       #`(let ((t e))
	   #,(let f ((c #'c) (c* #'(c* ...)))
	       (if (null? c*)
		   (syntax-case c (=> else)
		     ((else => e)
		      #'(e t))
		     ((else e e* ...)
		      #'(begin e e* ...))
		     (((k* ...) e e* ...)
		      #'(if (memv t '(k* ...)) (begin e e* ...))))
                   (with-syntax ((next (f (car c*) (cdr c*))))
                     (syntax-case c (=>)
                       (((k* ...) => e)
                        #'(let ((x (memv t '(k* ...))))
                            (if x (e (car x)) next)))
                       (((k* ...) e e* ...)
                        #'(if (memv t '(k* ...))
                              (begin e e* ...)
                              next)))))))))))

(define-syntax cond
  (lambda (stx)
    (syntax-case stx ()
      ((_ c c* ...)
       (let f ((c #'c) (c* #'(c* ...)))
         (if (null? c*)
             (syntax-case c (else)
               ((else e e* ...)
                #'(begin e e* ...))
               ((t e e* ...)
                #'(if t (begin e e* ...)))
               ((t)
                #'t))
             (with-syntax ((next (f (car c*) (cdr c*))))
               (syntax-case c (=>)
                 ((t => e)
                  #'(let ((x t))
                      (if x (e x) next)))
                 ((t e e* ...)
                  #'(if t (begin e e* ...) next))
                 ((t)
                  #'(or t next))))))))))

(define-syntax do
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((v* i* . s**) ...) (t e* ...) c* ...)
       (with-syntax
	   (((s* ...)
	     (map (lambda (s* v)
		    (syntax-case s* ()
		      ((s) #'s)
		      (() v)
		      (_ (raise-syntax-error stx "ill-formed do form"))))
		  #'(s** ...) #'(v* ...))))
	 #'(let f ((v* i*) ...)
             (if t
                 (begin (if #f #f)
                        e* ...)
                 (begin c* ...
                        (f s* ...)))))))))

(define-syntax guard
  ;; TODO: Rewrite using with-syntax.
  (lambda (stx)
    (syntax-case stx ()
      ((_ (v c c* ...) . b)
       #`((call/cc
	   (lambda (guard-k)
	     (with-exception-handler
		 (lambda (condition)
		   ((call/cc
		     (lambda (handler-k)
		       (guard-k
			(lambda ()
			  (let ((v condition))
			    #,(let f ((c #'c) (c* #'(c* ...)))
				(if (null? c*)
				    (syntax-case c (else)
				      ((else e e* ...)
				       #'(begin e e* ...))
				      ((t e e* ...)
				       #'(if t
					     (begin e e* ...)
					     (handler-k
					      (lambda ()
						(raise-continuable
						 condition)))))
				      ((t)
				       #'(let ((u t))
					   (if u
					       u
					       (handler-k
						(lambda ()
						  (raise-continuable
						   condition)))))))
				    (syntax-case c (=>)
				      ((t => e)
				       #`(let ((x t))
					   (if x
					       (e x)
					       #,(f (car c*) (cdr c*)))))
				      ((t e e* ...)
				       #`(if t
					     (begin e e* ...)
					     #,(f (car c*) (cdr c*))))
				      ((t)
				       #`(let ((u t))
					   (or u
					       #,(f (car c*)
                                                    (cdr c*)))))))))))))))
	       (lambda ()
		 (let-values ((args (letrec* () . b)))
		   (guard-k
		    (lambda ()
		      (apply values args)))))))))))))

(define-syntax let*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((id* init*) ...) . body)
       (let ((id* #'(id* ...))
	     (init* #'(init* ...)))
	 (if (null? id*)
	     #'(letrec* () . body)
	     (let f ((id (car id*)) (init (car init*))
		     (id* (cdr id*)) (init* (cdr init*)))
	       (if (null? id*)
		   #`(let ((#,id #,init)) . body)
		   #`(let ((#,id #,init))
		       #,(f (car id*) (car init*)
			    (cdr id*) (cdr init*))))))))
      (_
       (raise-syntax-error stx "ill-formed let* form")))))

(define-syntax let*-values
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((v* e*) ...) . b)
       (let ((v* #'(v* ...))
	     (e* #'(e* ...)))
	 (if (null? v*)
	     #'(letrec* () . b)
	     (let f ((v (car v*)) (e (car e*))
		     (v* (cdr v*)) (e* (cdr e*)))
	       (if (null? v*)
		   #`(let-values ((#,v #,e)) . b)
		   #`(let-values ((#,v #,e))
		       #,(f (car v*) (car e*)
			    (cdr v*) (cdr e*))))))))
      (_
       (raise-syntax-error stx "ill-formed let*-values form")))))

(define-syntax when
  (lambda (stx)
    (syntax-case stx ()
      ((_ t e e* ...)
       #'(if t (begin e e* ...)))
      (_
       (raise-syntax-error stx "ill-formed when form")))))

(define-syntax unless
  (lambda (stx)
    (syntax-case stx ()
      ((_ t e e* ...)
       #'(if (not t) (begin e e* ...)))
      (_
       (raise-syntax-error stx "ill-formed unless form")))))

;;;;;;;;;;;;;;;;;;;;;
;; Quasi-Quotation ;;
;;;;;;;;;;;;;;;;;;;;;

;; TODO: More than one entry after unquote-splicing.
;; TODO: Implement the remaining patterns.

(define-syntax quasiquote
  (lambda (stx)
    (syntax-case stx ()
      ((_ t)
       (let-values
           (((e v?)
             (let f ((t #'t) (n 0))
               (define (f* t* n)
                 (if (null? t*)
                     (values '() #f)
                     (let-values (((e1 v1?) (f (car t*) n))
                                  ((e2* v2?) (f* (cdr t*) n)))
                       (values (cons e1 e2*)
                               (or v1? v2?)))))
               (with-syntax ((t t))
                 (define (fail)
                   (error "ill-formed quasiquote template" #'t))
                 (syntax-case #'t (quasiquote unquote unquote-splicing)
                   ((unquote t1)
                    (if (zero? n)
                        (values #'t1 #t)
                        (let-values (((e v?) (f #'t1 (- n 1))))
                          (with-syntax ((e e))
                            (if v?
                                (values #'(list 'unquote e) #t)
                                (values #''t #f))))))
                   ((unquote t1* ...)
                    (fail))
                   ((unquote-splicing t1* ...)
                    (fail))
                   ;; check whether we can simple add a pattern zero? here.
                   (((unquote t1* ...) . t2)
                    (if (zero? n)
                        (let-values (((e2 v2?) (f #'t2 0)))
                          (with-syntax ((e2 e2))
                            (values #'(cons* t1* ... e2) #t)))
                        (let-values (((e1* v1?) (f* #'(t1* ...) (- n 1)))
                                     ((e2 v2?) (f #'t2 n)))
                          (with-syntax (((e1* ...) e1*)
                                        (e2 e2))
                            (if (or v1? v2?)
                                (values #'(cons (list 'unquote e1* ...) e2) #t)
                                (values #''t #f))))))
                   (((unquote-splicing t1* ...) . t2)
                    (if (zero? n)
                        (let-values (((e2 v2?) (f #'t2 0)))
                          (with-syntax ((e2 e2))
                            (values #'(append t1* ... e2) #t)))
                        (let-values (((e1* v1?) (f* #'(t1* ...) (- n 1)))
                                     ((e2 v2?) (f #'t2 n)))
                          (with-syntax (((e1* ...) e1*)
                                        (e2 e2))
                            (if (or v1? v2?)
                                (values #'(cons (list 'unquote-splcing e1* ...)
                                                e2) #t)
                                (values #''t #f))))))
                   ((quasiquote t1)
                    (let-values (((e v?) (f #'t1 (+ n 1))))
                      (with-syntax ((e e))
                        (if v?
                            (values #'(list 'quasiquote e) #t)
                            (values #''t #f)))))
                   ((quasiquote t1 ...)
                    (fail))
                   (#(t1* ...)
                    (let-values (((e v?) (f #'(t1* ...) n)))
                      (if v?
                          (with-syntax ((e e))
                            (values #'(list->vector e) #t))
                          (values #''t #f))))
                   ((t1 . t2)
                    (let-values (((e1 v1?) (f #'t1 n))
                                 ((e2 v2?) (f #'t2 n)))
                      (with-syntax ((e1 e1) (e2 e2))
                        (if (or v1? v2?)
                            (values #'(cons e1 e2) #t)
                            (values #''t #f)))))
                   (t
                    (let ((v (syntax->datum #'t)))
                      (or (boolean? v)
                          (number? v)
                          (char? v)
                          (null? v)
                          (string? v)
                          (symbol? v)
                          (bytevector? v)))
                    (values #''t #f))
                   (_
                    (raise-syntax-error #'t
                                        "ill-formed quasiquote template")))))))
	 e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-splicing binding constructs for syntactic keywords ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax let-syntax
  (lambda (stx)
    (syntax-case stx ()
      ((_ b . body)
       #'(splicing-let-syntax b (letrec* () . body))))))

(define-syntax letrec-syntax
  (lambda (stx)
    (syntax-case stx ()
      ((_ b . body)
       #'(splicing-letrec-syntax b (letrec* () . body))))))

;;;;;;;;;;;;;;;;;;
;; Syntax-Rules ;;
;;;;;;;;;;;;;;;;;;

(define-syntax syntax-rules
  (lambda (stx)
    (syntax-case stx ()
      ((_ ell lit* ((k* . p*) t*) ...)
       (and (identifier? #'ell) (every identifier? #'(k* ...)))
       #'(lambda (stx)
	   (with-ellipsis ell
	     (syntax-case stx lit*
	       ((_ . p*) #'t*) ...))))
      ((_ lit* ((k* . p*) t*) ...)
       (every identifier? #'(k* ...))
       #'(lambda (stx)
	   (syntax-case stx lit*
	     ((_ . p*) #'t*) ...)))
      (_
       (raise-syntax-error stx "ill-formed syntax-rules transformer")))))

(define-syntax syntax-error
  (lambda (stx)
    (syntax-case stx ()
      ((_ s e* ...)
       (apply error (syntax->datum #'s) stx
	      (map syntax->datum #'(e* ...))))
      (_
       (raise-syntax-error stx "ill-formed syntax-error form")))))
