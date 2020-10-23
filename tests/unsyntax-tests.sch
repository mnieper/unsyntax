;; Copyright © Marc Nieper-Wißkirchen (2020).

;; This file is part of Unsyntax.

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
        (unsyntax)
        (srfi :211 syntax-case)
        (srfi :64))

(test-begin "Unsyntax Extensions Tests")

(test-group "Meta Definitions"
  (meta define x #'41)
  (meta set! x #'42)
  (define-syntax foo
    (lambda (stx)
      x))

  (meta define-record-type <rtd>
    (make-record a b)
    record?
    (a a a-set!)
    (b b))

  (define-syntax bar
    (lambda (stx)
      (syntax-case stx ()
        ((k)
         (let ((r (make-record 1 2)))
           (a-set! r 3)
           #`(quote #,(datum->syntax #'k (list (a r) (b r)))))))))

  (test-equal 42 foo)
  (test-equal '(3 2) (bar)))

(test-group "Meta Definition Examples"
  (meta define (construct-name key . args)
    (datum->syntax key
                   (string->symbol
                    (apply string-append
                           (map (lambda (x)
                                  (cond ((string? x) x)
                                        ((identifier? x)
                                         (symbol->string (syntax->datum x)))))
                                args)))))

  (define-syntax define-zoo
    (lambda (stx)
      (syntax-case stx ()
        ((k prefix)
         #`(begin
             (define #,(construct-name #'k #'prefix "-lion") 'lion)
             (define #,(construct-name #'k #'prefix "-tiger") 'tiger))))))

  (define-zoo local)

  (test-equal 'lion local-lion)
  (test-equal 'tiger local-tiger))

(test-group "Modules"
  (test-equal 42
    (let* ()
      (module (a)
        (define b 42)
        (define (a) b))
      (define b 41)
      (a)))

  (test-equal 'meta
    (let* ()
      (module (id)
        (define x 'meta)
        (meta module (id)
          (define id #'x)))
      (define-syntax foo
        (lambda (stx)
          id))
      foo))

  (test-equal 'apple
    (let* ()
      (module (define-apple)
          (define apple 'apple)
        (define-syntax define-apple
          (syntax-rules ()
            ((define-apple id)
             (define id apple)))))
      (define-apple apple)
      apple)))

(test-end)
