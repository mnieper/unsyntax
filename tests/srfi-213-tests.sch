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
        (srfi 211 syntax-case)
        (srfi 213))

(define-syntax get-property
  (lambda (stx)
    (lambda (lookup)
      (syntax-case stx ()
        ((_ id key)
         #`'#,(datum->syntax #'* (lookup #'id #'key)))))))

(test-begin "SRFI 213")

(define info)
(define x "x-value")
(define-property x info "x-info")

(test-equal "x-value" x)
(test-equal "x-info" (get-property x info))

(test-equal #f (let ((x 0))
                 (get-property x info)))

(test-equal "x-new-info" (let* ()
                           (define-property x info "x-new-info")
                           (get-property x info)))

(test-assert
    (let ()
      (define y)
      (define-property x info #'id)
      (define-property y info #'id)
      (let-syntax ((get (lambda (stx)
                          (lambda (p)
                            (bound-identifier=? (p #'x #'info)
                                                (p #'y #'info))))))
        get)))

(test-end)
