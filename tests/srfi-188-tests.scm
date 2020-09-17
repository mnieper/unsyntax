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
        (srfi 188))

(test-begin "Splicing binding constructs for syntactic keywords")

(test-equal 1
  (let ((x #t))
    (splicing-let-syntax ()
      (define x 1)
      #f)
    x))

(test-equal 2
  (let ((x #t))
    (splicing-let-syntax ()
      (define x 2))
    x))

(test-equal 3
  (let ((x #t))
    (splicing-let-syntax
        ((f (syntax-rules ()
              ((f) x))))
      (define x 3)
      (f))))

(test-equal 4
  (let ((x #t))
    (splicing-letrec-syntax ()
      (define x 4)
      #f)
    x))

(test-equal 5
  (let ((x #t))
    (splicing-letrec-syntax ()
      (define x 5))
    x))

(test-equal 6
  (let ((x #t))
    (splicing-letrec-syntax
        ((f (syntax-rules ()
              ((f) x))))
      (define x 6)
      (f))))

(test-equal 'now
  (splicing-let-syntax ((given-that (syntax-rules ()
                                      ((given-that test stmt1 stmt2 ...)
                                       (if test
                                           (begin stmt1
                                                  stmt2 ...))))))
    (let ((if #t))
      (given-that if (set! if 'now))
      if)))

(test-equal 'outer
  (let ((x 'outer))
    (splicing-let-syntax ((m (syntax-rules () ((m) x))))
      (let ((x 'inner))
        (m)))))

(test-equal 7
  (splicing-letrec-syntax
      ((my-or (syntax-rules ()
                ((my-or) #f)
                ((my-or e) e)
                ((my-or e1 e2 ...)
                 (let ((temp e1))
                   (if temp
                       temp
                       (my-or e2 ...)))))))
    (let ((x #f)
          (y 7)
          (temp 8)
          (let odd?)
          (if even?))
      (my-or x
             (let temp)
             (if y)
             y))))

(test-equal 'let-syntax
  (let ((x 'let-syntax))
    (let-syntax ()
      (define x 'splicing-let-syntax)
      #f)
    x))

(test-equal 'splicing-let-syntax
  (let ((x 'let-syntax))
    (splicing-let-syntax ()
      (define x 'splicing-let-syntax)
      #f)
    x))

(test-end)
