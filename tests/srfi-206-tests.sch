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
        (srfi 139)
	(srfi 206)
        (srfi 211 syntax-case)
        (srfi 64)
        (rename (only (srfi 206 all) foo bar) (bar baz)))

(test-begin "SRFI 206")

(test-assert
    (let* ()
      (define-auxiliary-syntax foo foo)
      (define-syntax is-foo?
        (syntax-rules (foo)
          ((_ foo) #t)
          ((_ _) #f)))
      (let* ()
        (is-foo? foo))))

(test-assert
    (let* ()
      (define-auxiliary-syntax foo foo)
      (define-syntax is-foo?
        (syntax-rules (foo)
          ((_ foo) #t)
          ((_ _) #f)))
      (let* ()
        (define-auxiliary-syntax bar foo)
        (is-foo? bar))))

(test-assert
    (not
     (let* ()
       (define-auxiliary-syntax foo foo)
       (define-syntax is-foo?
         (syntax-rules (foo)
           ((_ foo) #t)
           ((_ _) #f)))
       (let ()
         (define-syntax foo (syntax-rules ()))
         (is-foo? foo)))))

(test-equal '((+ 1 2) 3)
  (syntax-parameterize
      ((unquote
        (syntax-rules ()
          ((_ e) (eval e (environment '(scheme base)))))))
    (let ((x '(+ 1 2)))
      (list `,x ,x))))

(test-assert
    (let* ()
      (define-auxiliary-syntax foo2 foo)
      (free-identifier=? #'foo2 #'foo)))

(test-assert
    (let* ()
      (define-auxiliary-syntax foo2 foo)
      (free-identifier=? #'foo2 #'foo)))

(test-assert
    (let* ()
      (define-auxiliary-syntax bar2 bar)
      (not (free-identifier=? #'bar2 #'bar))))

(test-assert
    (let* ()
      (define-auxiliary-syntax baz baz)
      (not (free-identifier=? #'baz #'bar))))

(test-end)
