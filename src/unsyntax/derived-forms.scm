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

(define-syntax lambda
  (case-lambda
    ((stx)
     (syntax-case stx ()
       ((_ formals . body)
        #'(case-lambda (formals . body)))))))

(define-syntax define
  (lambda (stx)
    (syntax-case stx ()
      ((_ var expr)
       (identifier? #'var)
       #'(define-values (var) expr))
      ((_ var)
       (identifier? #'var)
       #'(define-values (var) (if #f #f)))
      ((_ (var . formals) . body)
       (identifier? #'var)
       #'(define var (lambda formals . body))))))

(define-syntax let
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((name* val*) ...) . body)
       #'((lambda (name* ...) . body) val* ...))
      ((_ tag ((name* val*) ...) . body)
       (identifier? #'tag)
       #'(letrec* ((tag (lambda (name* ...) . body)))
	   (tag val* ...)))
      (_
       (raise-syntax-error stx "ill-formed let form")))))

(define-syntax and
  (lambda (stx)
    (syntax-case stx ()
      ((_)
       #t)
      ((_ test)
       #'test)
      ((_ test test* ...)
       #'(if test
	     (and test* ...)
	     #f))
      (_
       (raise-syntax-error stx "ill-formed and form")))))

(define-syntax or
  (lambda (stx)
    (syntax-case stx ()
      ((_)
       #f)
      ((_ test)
       #'test)
      ((_ test test* ...)
       #'(let ((t test))
	   (if t t (or test* ...))))
      (_
       (raise-syntax-error stx "ill-formed or form")))))

(define-syntax with-syntax
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((p* e*) ...) body1 body2 ...)
       #'(syntax-case (list e* ...) ()
           ((p* ...) (letrec* () body1 body2 ...)))))))
