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
	(srfi 1)
	(srfi 64))

(test-begin "SRFI 1")

(test-group "Constructors"
  (test-equal '(1 2 3 . 4) (cons* 1 2 3 4))
  (test-equal 1 (cons* 1)))

(test-group "Predicates"
  (test-eq #f (not-pair? '(a . b)))
  (test-eq #f (not-pair? '(a b c)))
  (test-eq #t (not-pair? '()))
  (test-eq #t (not-pair? '#(a b)))
  (test-eq #t (not-pair? 7))
  (test-eq #t (not-pair? 'a)))

(test-group "Miscellaneous: length, append, concatenate, reverse, zip
& count"
  (test-equal '(x y) (concatenate '((x) (y))))
  (test-equal '(a b c d) (concatenate '((a) (b c d))))
  (test-equal '(a (b) (c)) (concatenate '((a (b)) ((c)))))
  (test-equal '(a b c . d) (concatenate '((a b) (c . d))))
  (test-equal 'a (concatenate '(() a)))
  (test-equal '(x y) (concatenate '((x y))))
  (test-equal '() (concatenate '()))

  (test-equal '(c b a) (reverse! (list 'a ' b 'c)))
  (test-equal '((e (f)) d (b c) a) (reverse! (list 'a '(b c) 'd '(e (f)))))

  (test-equal '(x y) (append-reverse! (list 'x) '(y)))
  (test-equal '(a b c d) (append-reverse! (list 'a) '(b c d)))
  (test-equal '((b) a (c)) (append-reverse! (list 'a '(b)) '((c))))
  (test-equal '(b a c . d) (append-reverse! (list 'a 'b) '(c . d)))
  (test-equal 'a (append-reverse! '() 'a)))

(test-group "Fold, unfold & map"
  (test-equal '(c 3 b 2 a 1) (fold cons* '() '(a b c) '(1 2 3 4 5)))

  (test-equal '(a 1 b 2 c 3) (fold-right cons* '() '(a b c) '(1 2 3 4 5)))

  (test-equal '((a b c) (b c) (c)) (pair-fold-right cons '() '(a b c)))

  (test-equal '(1 4 9 16 25 36 49 64 81 100)
    (unfold (lambda (x) (> x 10))
	    (lambda (x) (* x x))
	    (lambda (x) (+ x 1))
	    1))

  (test-equal '(1 -1 3 -3 8 -8)
    (append-map (lambda (x) (list x (- x))) '(1 3 8)))

  (test-equal '(1 -1 3 -3 8 -8)
    (append-map! (lambda (x) (list x (- x))) '(1 3 8))))

(test-group "Filtering & partitioning"
  (test-equal '(0 8 8 -4) (filter! even? (list 0 7 8 8 43 -4))))

(test-group "Searching"
  (test-equal 4 (find even? '(3 1 4 1 5 9)))

  (test-equal '(-8 -5 0 0) (find-tail even? '(3 1 37 -8 -5 0 0)))
  (test-equal #f (find-tail even? '(3 1 37 -5)))

  (test-equal #t (any integer? '(a 3 b 2.7)))
  (test-equal #f (any integer? '(a 3.1 b 2.7)))
  (test-equal #t (any < '(3 1 4 1 5) '(2 7 1 8 2)))

  (test-equal 2 (list-index even? '(3 1 4 1 5 9)))
  (test-equal 1 (list-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)))
  (test-equal #f (list-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))))

(test-group "Set operations on lists"
  (test-equal '(u o i a b c d c e)
    (lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o 'u))

  (test-equal '(b c d)
    (lset-difference! eq? (list 'a 'b 'c 'd 'e) '(a e i o u)))
  (test-equal '(a b c)
    (lset-difference! eq? (list 'a 'b 'c))))

(test-end)
