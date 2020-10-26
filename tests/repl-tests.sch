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
        (scheme repl)
        (srfi 64))

(define-syntax repl
  (syntax-rules ()
    ((repl expr)
     (eval 'expr (interaction-environment)))))

(define-syntax test-repl
  (syntax-rules ()
    ((test-repl out expr)
     (test-equal out
       (eval 'expr (interaction-environment))))))


(test-begin "REPL")

(test-repl 42 42)

(test-repl 3 (begin 1 2 3))

(repl (define x 10))
(test-repl 10 x)
(repl (set! y 11))
(test-repl 11 y)

(repl (define-values (a . b) (values 1 2 3)))
(test-repl 1 a)
(test-repl '(2 3) b)

(repl (import (srfi :212)))
(repl (alias z y))
(repl (set! z 1))
(test-repl 1 y)

(repl (import (srfi :211 identifier-syntax)))
(repl (define-syntax foo (identifier-syntax 2)))
(test-repl 2 foo)

(repl (import (srfi :188)))
(test-repl 4 (begin (splicing-let-syntax ((foo (identifier-syntax 4)))
                      (define x foo))
                    x))

(repl (import (srfi :211 syntax-case)
              (srfi :211 with-ellipsis)))
(test-repl '(a b c) (with-ellipsis :::
                      (syntax-case #'(a b c) ()
                        ((x :::) (syntax->datum #'(x :::))))))

(repl (define-record-type <record>
	(make-record a)
	record?
	(a get-a)
	(b get-b set-b!)))

(test-repl #t (record? (make-record 'a)))
(test-repl 'a (get-a (make-record 'a)))
(test-repl 'b (let ((record (make-record 'a)))
		(set-b! record 'b)
		(get-b record)))

(test-end)
