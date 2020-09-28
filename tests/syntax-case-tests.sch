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

(import (scheme base)
	(srfi 64)
        (srfi 211 identifier-syntax)
	(srfi 211 syntax-case)
        (srfi 211 variable-transformer)
        (srfi 211 with-ellipsis))

(test-begin "syntax-case")

(test-begin "Base library")

(test-group "11.18"
  (define p (cons 4 5))
  (define-syntax p.car
    (identifier-syntax
     (_ (car p))
     ((set! _ e) (set-car! p e))))

  (set! p.car 15)
  (test-equal 15 p.car)
  (test-equal '(15 . 5) p))

(test-end "Base library")

(test-begin "syntax-case")

(test-group "12.4"
  (define p (cons 4 5))

  (define-syntax p.car
    (make-variable-transformer
     (lambda (x)
      (syntax-case x (set!)
        ((set! _ e) #'(set-car! p e))
        ((_ . rest) #'((car p) . rest))
        (_  #'(car p))))))

  (test-equal 4 p.car)

  (set! p.car 15)
  (test-equal 15 p.car)
  (test-equal '(15 . 5) p))

(test-group "with-ellipsis"
  (define-syntax define-quotation-macros
      (lambda (x)
        (syntax-case x ()
          ((_ (macro-name head-symbol) ...)
           #'(begin (define-syntax macro-name
                      (lambda (x)
                        (with-ellipsis :::
                          (syntax-case x ()
                            ((_ x :::)
                             #'(quote (head-symbol x :::)))))))
                    ...)))))

  (define-quotation-macros (quote-a a) (quote-b b) (quote-c c))

  (test-equal '(a 1 2 3) (quote-a 1 2 3)))

(test-end "syntax-case")

(test-end "syntax-case")
