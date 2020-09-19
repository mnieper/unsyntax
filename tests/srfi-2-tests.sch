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
	(srfi 2)
	(srfi 64))

(eval '(import (srfi 2)) (interaction-environment))

(test-begin "SRFI 2")

(test-equal 1 (and-let* () 1))
(test-equal 2  (and-let* () 1 2))
(test-equal #t (and-let* ()))

(test-equal #f (let ((x #f))
		 (and-let* (x))))
(test-equal 1 (let ((x 1))
		(and-let* (x))))
(test-equal #f
  (and-let* ((x #f))))
(test-equal 1 (and-let* ((x 1))))
(test-error (test-read-eval-string "(and-let* (#f (x 1)))"))
(test-equal #f (and-let* ((#f)
			  (x 1))))
(test-error (test-read-eval-string "(and-let* (2 (x 1)))"))
(test-equal 1 (and-let* ((2) (x 1))))
(test-equal 2 (and-let* ((x 1) (2))))
(test-equal #f (let ((x #f))
		 (and-let* (x) x)))
(test-equal "" (let ((x ""))
		 (and-let* (x) x)))
(test-equal "" (let ((x ""))
		 (and-let* (x))))
(test-equal 2 (let ((x 1))
		(and-let* (x) (+ x 1))))
(test-equal #f (let ((x #f))
		 (and-let* (x) (+ x 1))))
(test-equal 2 (let ((x 1))
		(and-let* (((positive? x))) (+ x 1))))
(test-equal #t (let ((x 1))
		 (and-let* (((positive? x))))))
(test-equal #f (let ((x 0))
		 (and-let* (((positive? x))) (+ x 1))))
(test-equal 3 (let ((x 1))
		(and-let* (((positive? x)) (x (+ x 1))) (+ x 1))))

(test-skip 1)
(test-error
 (test-read-eval-string "(let ((x 1))
			  (and-let* (((positive? x)) (x (+ x 1)) (x (+ x 1)))
			    (+ x 1)))"))

(test-equal 2 (let ((x 1))
		(and-let* (x ((positive? x))) (+ x 1))))
(test-equal 2 (let ((x 1))
		(and-let* ( ((begin x)) ((positive? x))) (+ x 1))))
(test-equal #f (let ((x 0))
		 (and-let* (x ((positive? x))) (+ x 1))))
(test-equal #f (let ((x #f))
		 (and-let* (x ((positive? x))) (+ x 1))))
(test-equal #f (let ((x #f))
		 (and-let* ( ((begin x)) ((positive? x))) (+ x 1))))

(test-equal #f (let ((x 1))
		 (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
(test-equal #f (let ((x 0))
		 (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
(test-equal #f (let ((x #f))
		 (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
(test-equal 3/2 (let ((x 3))
		  (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))

(test-end)
