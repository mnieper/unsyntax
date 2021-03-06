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
        (scheme eval)
        (srfi 64)
        (srfi 211 syntax-case)
        (srfi 213)
        (example library)
	(example library-import-only)
        (for (example r6rs-library ((<= 6)))
          run expand (meta -1))
        (rename (unsyntax) (import import-module)))
(import (rename (scheme base) (define import)))

(import quux 'import)

(test-begin "Compiler Test")

(test-equal 'import quux)

(test-assert (memq 'test (features)))

(test-equal 42 foo)
(test-equal 'bar (bar))

(test-equal 42 (eval 'foo (environment '(example library))))
(test-equal 'bar (eval '(bar) (environment '(example library))))

(test-equal "the-answer"
  (let-syntax ((get-the-answer
                (lambda (stx)
                  (lambda (lookup)
                    #`'#,(datum->syntax #'* (lookup #'foo #'*))))))
    (get-the-answer)))
(test-equal "the-answer"
  (eval '(let-syntax ((get-the-answer
                       (lambda (stx)
                         (lambda (lookup)
                           #`'#,(datum->syntax #'* (lookup #'foo #'*))))))
           (get-the-answer))
        (environment '(scheme base) '(example library)
                     '(srfi 211 syntax-case))))

(test-equal 42
  (let-syntax ((foo (lambda (stx)
                      meta-foo)))
    foo))

(test-equal 'apple
  (let ()
    (define-apple apple)
    apple))

(test-equal 'pear
  (let ()
    (import-module fruits)
    pear))

(test-equal 'after-barrier barrier-x)

(test-equal 3
  (let* ()
    (import-module (rename (library (scheme base)) (+ plus)))
    (plus 1 2)))

(test-equal 9
  (let* ()
    (import-module (example library-local))
    (sq 3)))

(test-end)
