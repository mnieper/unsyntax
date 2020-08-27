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

(define (build-command loc cmd)
  (build-define-values loc
                       (build-formals loc
                                      (list (build-reference
                                             loc (generate-variable "_")))
                                      #t)
                       cmd))

(define (build-define loc ref init)
  (build-define-values loc (build-formals loc (list ref) #f) init))

(define (build-global-assignment loc ref expr)
  (build-primitive-call loc 'set-box! (list ref expr)))

(define (build-global-reference loc var)
  (build-primitive-call loc 'unbox (list var)))

(define (build-lambda loc formals body)
  (build-case-lambda loc (list formals) (list body)))

(define (build-let loc refs inits expr)
  (build-call loc
              (build-lambda loc (build-formals loc refs #f) expr)
              inits))

(define (build-letrec* loc refs inits expr)
  (build-body loc
              (map (lambda (ref init)
                     (build-define loc ref init))
                   refs inits)
              expr))

(define (build-named-let loc name refs inits expr)
  (build-body loc
	      (list (build-define loc
				  (build-reference loc name)
				  (build-lambda loc
						(build-formals loc refs #f)
						expr)))
	      (build-call loc (build-reference loc name) inits)))

(define (build-primitive-call loc prim args)
  (build-call loc (build-primitive loc prim) args))

(define (build-thunk loc expr)
  (build-lambda loc (build-formals loc '() #f) expr))

(define-syntax build
  (syntax-rules ()
    ((build loc e)
     (let ((%loc loc))
       (%build %loc e)))))

(define-syntax %build
  (syntax-rules ::: (...
                     >=
                     -
                     append
                     car
                     case-lambda
                     cdr
                     cons
                     equal?
                     fold-right
                     free-identifier=?
                     if
                     lambda
                     let
                     list
                     list->vector
                     null?
                     raise-syntax-error
                     receive
                     reverse
                     quote
                     set-global!
                     syntax-car
                     syntax-cdr
                     syntax->datum
                     syntax->list
                     syntax-length+
                     syntax-null?
                     syntax-pair?
                     syntax-split-at
                     syntax-vector?
                     syntax-vector->list
                     unquote
                     unquote-splicing)
    ((_ loc ',e)
     (build-literal loc e))
    ((_ loc 'datum)
     (build-literal loc 'datum))
    ((_ loc ,e) e)
    ((_ loc (if t c))
     (build-conditional loc (%build loc t) (%build loc c)))
    ((_ loc (if t c a))
     (build-conditional loc (%build loc t) (%build loc c) (%build loc a)))
    ((_ loc (case-lambda (formals* init*) :::))
     (build-case-lambda loc
                        (list (%build-formals loc formals*) :::)
                        (list (%build loc init*) :::)))
    ((_ loc (lambda () body))
     (build-thunk loc (%build loc body)))
    ((_ loc (lambda (x* ... y) z))
     (build-lambda loc
                   (build-formals loc
                                  (append (map (lambda (x)
                                                 (build-reference loc x))
                                               x*)
                                          (list (build-reference loc y)))
                                  #f)
                   (%build loc z)))
    ((_ loc (let ((var* init*) :::) expr))
     (build-let loc
                (list (build-reference loc var*) :::)
                (list (%build loc init*) :::)
                (%build loc expr)))
    ((_ loc (let var ((var* init*) ::: (var2* ,init2*) ...) expr))
     (build-named-let loc
                      (build-reference loc var)
                      (cons* (build-reference loc var*) :::
                             (map (lambda (v) (build-reference loc v)) var2*))
                      (cons* (%build loc init*) ::: init2*)
                      (%build loc expr)))
   ((_ loc (receive formals init expr))	;TODO: Write build-receive.
     (build-body loc
		(list (build-define-values loc
					  (%build-formals loc formals)
					  (%build loc init)))
		(%build loc expr)))
    ((_ loc (>= x* :::))
     (build-primitive-call loc '>= (list (%build loc x*) :::)))
    ((_ loc (- x* :::))
     (build-primitive-call loc '- (list (%build loc x*) :::)))
    ((_ loc (append x y))
     (build-primitive-call loc 'append (list (%build loc x) (%build loc y))))
    ((_ loc (car x))
     (build-primitive-call loc 'car (list (%build loc x))))
    ((_ loc (cdr x))
     (build-primitive-call loc 'cdr (list (%build loc x))))
    ((_ loc (cons x y))
     (build-primitive-call loc 'cons (list (%build loc x) (%build loc y))))
    ((_ loc (equal? x y))
     (build-primitive-call loc 'equal? (list (%build loc x) (%build loc y))))
    ((_ loc (free-identifier=? x y))
     (build-primitive-call loc 'free-identifier=? (list (%build loc x)
                                                        (%build loc y))))
    ((_ loc (fold-right p x* ::: ,y ...))
     (build-primitive-call loc 'fold-right (cons* (%build loc p)
						 (%build loc x*) :::
						 y)))
    ((_ loc (list x* :::))
     (build-primitive-call loc 'list (list (%build loc x*) :::)))
    ((_ loc (list->vector x))
     (build-primitive-call loc 'list->vector (list (%build loc x))))
    ((_ loc (null? x))
     (build-primitive-call loc 'null? (list (%build loc x))))
    ((_ loc (raise-syntax-error x x* :::))
     (build-primitive-call loc 'raise-syntax-error (list (%build loc x)
                                                         (%build loc x*) :::)))
    ((_ loc (reverse x))
     (build-primitive-call loc 'reverse (list (%build loc x))))
    ((_ loc (set-global! x y))
     (build-primitive-call loc 'set-global! (list (%build loc x)
                                                  (%build loc y))))
    ((_ loc (set-keyword! x y))
     (build-primitive-call loc 'set-keyword! (list (%build loc x)
                                                   (%build loc y))))
    ((_ loc (syntax->datum x))
     (build-primitive-call loc 'syntax->datum (list (%build loc x))))
    ((_ loc (syntax->list x))
     (build-primitive-call loc 'syntax->list (list (%build loc x))))
    ((_ loc (syntax-car x))
     (build-primitive-call loc 'syntax-car (list (%build loc x))))
    ((_ loc (syntax-cdr x))
     (build-primitive-call loc 'syntax-cdr (list (%build loc x))))
    ((_ loc (syntax-length+ x))
     (build-primitive-call loc 'syntax-length+ (list (%build loc x))))
    ((_ loc (syntax-null? x))
     (build-primitive-call loc 'syntax-null? (list (%build loc x))))
    ((_ loc (syntax-pair? x))
     (build-primitive-call loc 'syntax-pair? (list (%build loc x))))
    ((_ loc (syntax-split-at x y))
     (build-primitive-call loc 'syntax-split-at (list (%build loc x)
                                                      (%build loc y))))
    ((_ loc (syntax-vector? x))
     (build-primitive-call loc 'syntax-vector? (list (%build loc x))))
    ((_ loc (syntax-vector->list x))
     (build-primitive-call loc 'syntax-vector->list (list (%build loc x))))
    ((_ loc (f x* ::: ,y ...))
     (build-call loc (%build loc f) (cons* (%build loc x*) ::: y)))
    ((_ loc (f x* :::))
     (build-call loc (%build loc f) (list (%build loc x*) :::)))
    ((_ loc x) (build-reference loc x))))

(define-syntax %build-formals
  (syntax-rules ()
    ((_ loc (x* ...))
     (build-formals loc (list (build-reference loc x*) ...) #f))
    ((_ loc (x* ... . y*))
     (build-formals loc (list (build-reference loc x*) ...
                              (build-reference loc y*)) #t))))
