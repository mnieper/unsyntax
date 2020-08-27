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
	(scheme case-lambda)
	(scheme eval)
	(scheme inexact)
	(scheme lazy)
	(scheme r5rs)
	(scheme write)
	(srfi 64))

(define-syntax test-output
  (syntax-rules ()
    ((test-output out expr)
     (test-equal out
       (parameterize
	   ((current-output-port
	     (open-output-string)))
	 expr
	 (get-output-string (current-output-port)))))))

(test-begin "R7RS")

(test-group "2.4"
  ;; TODO
  (test-equal '#0=(a b c . #0#) (let ((x (list 'a 'b 'c)))
				  (set-cdr! (cddr x) x)
				  x)))

(test-begin "4.1")

(test-group "4.1.1"
  (define x 28)
  (test-equal 28 x))

(test-group "4.1.2"
  (test-equal 'a (quote a))
  (test-equal #(a b c) (quote #(a b c)))
  (test-equal '(+ 1 2) (quote (+ 1 2)))

  (test-equal 'a 'a)
  (test-equal #(a b c) '#(a b c))
  (test-equal '() '())
  (test-equal '(+ 1 2) '(+ 1 2))
  (test-equal 'a 'a)
  (test-equal '(quote a) ''a)

  (test-equal 145932 '145932)
  (test-equal 145932 145932)
  (test-equal "abc" '"abc")
  (test-equal "abc" "abc")
  (test-equal #\a '#\a)
  (test-equal #\a #\a)
  (test-equal #(a 10) '#(a 10))
  (test-equal #(a 10) #(a 10))
  (test-equal #u8(64 65) '#u8(64 65))
  (test-equal #u8(64 65) #u8(64 65))
  (test-equal #t '#t)
  (test-equal #t #t))

(test-group "4.1.3"
  (test-equal 7 (+ 3 4))
  (test-equal 12 ((if #f + *) 3 4)))
#;
(test-group "4.1.4"
  (define reverse-subtract
    (lambda (x y) (- y x)))

  (define add4
    (let ((x 4))
      (lambda (y) (+ x y))))

  (test-assert (procedure? (lambda (x) (+ x x))))
  (test-equal 8 ((lambda (x) (+ x x)) 4))

  (test-equal 3 (reverse-subtract 7 10))

  (test-equal 10 (add4 6))

  (test-equal '(3 4 5 6) ((lambda x x) 3 4 5 6))
  (test-equal '(5 6) ((lambda (x y . z) z) 3 4 5 6)))

(test-group "4.1.5"
  (test-equal 'yes (if (> 3 2) 'yes 'no))
  (test-equal 'no (if (> 2 3) 'yes 'no))
  (test-equal 1 (if (> 3 2) (- 3 2) (+ 3 2))))

(test-group "4.1.6"
  (define x 2)
  (test-equal 3 (+ x 1))
  (set! x 4)
  (test-equal 5 (+ x 1)))

(test-end)

(test-begin "4.2")

(test-group "4.2.1"
  (test-equal 'greater (cond ((> 3 2) 'greater)
			     ((< 3 2) 'less)))
  (test-equal 'equal (cond ((> 3 3) 'greater)
			   ((< 3 3) 'less)
			   (else 'equal)))
  (test-equal 2 (cond ((assv 'b '((a 1) (b 2))) => cadr)
		      (else #f)))

  (test-equal 'composite (case (* 2 3)
			   ((2 3 5 7) 'prime)
			   ((1 4 6 8 9) 'composite)))
  (test-equal 'c (case (car '(c d))
		   ((a e i o u) 'vowel)
		   ((w y) 'semivowel)
		   (else => (lambda (x) x))))

  (test-equal #t (and (= 2 2) (> 2 1)))
  (test-equal #f (and (= 2 2) (< 2 1)))
  (test-equal '(f g) (and 1 2 'c '(f g)))
  (test-equal #t (and))

  (test-equal #t (or (= 2 2) (> 2 1)))
  (test-equal #t (or (= 2 2) (< 2 1)))
  (test-equal #f (or #f #f #f))
  (test-equal '(b c) (or (memq 'b '(a b c))
			 (/ 3 0)))

  (test-output "12" (when (= 1 1.0)
		      (display "1")
		      (display "2")))
  (test-output "" (unless (= 1 1.0)
		    (display "1")
		    (display "2"))))

(test-group "4.2.2"
  (define (means ton)
    (letrec*
	((mean
	  (lambda (f g)
	    (f (/ (sum g ton) n))))
	 (sum
	  (lambda (g ton)
	    (if (null? ton)
		(+)
		(if (number? ton)
		    (g ton)
		    (+ (sum g (car ton))
		       (sum g (cdr ton)))))))
	 (n (sum (lambda (x) 1) ton)))
      (values (mean values values)
	      (mean exp log)
	      (mean / /))))

  (test-equal 6  (let ((x 2) (y 3))
		   (* x y)))
  (test-equal 35 (let ((x 2) (y 3))
		   (let ((x 7)
			 (z (+ x y)))
		     (* z x))))

  (test-equal 70 (let ((x 2) (y 3))
		   (let* ((x 7)
			  (z (+ x y)))
		     (* z x))))

  (test-equal #t
    (letrec ((even?
	      (lambda (n)
		(if (zero? n)
		    #t
		    (odd? (- n 1)))))
	     (odd?
	      (lambda (n)
		(if (zero? n)
		    #f
		    (even? (- n 1))))))
      (even? 88)))

  ;; TODO: The calculation should happen inside a test so errors are caught.
  (let*-values (((x y z) (means '(3 (1 4)))))
    (test-eqv 8/3 x)
    (test-approximate 2.28942848510666 1e-14 y)
    (test-eqv 36/19 z))

  (test-equal 35
    (let-values (((root rem) (exact-integer-sqrt 32)))
      (* root rem)))

  (test-equal '(x y x y)
    (let ((a 'a) (b 'b) (x 'x) (y 'y))
      (let*-values (((a b) (values x y))
		    ((x y) (values a b)))
	(list a b x y)))))

(test-group "4.2.3"
  (define x 0)

  (test-equal 6 (and (= x 0)
		     (begin (set! x 5)
			    (+ x 1))))

  (test-output "4 plus 1 equals 5"
    (begin (display "4 plus 1 equals ")
	   (display (+ 4 1)))))

(test-group "4.2.4"
  (test-equal #(0 1 2 3 4) (do ((vec (make-vector 5))
				(i 0 (+ i 1)))
			       ((= i 5) vec)
			     (vector-set! vec i i)))

  (test-equal 25
    (let ((x '(1 3 5 7 9)))
      (do ((x x (cdr x))
	   (sum 0 (+ sum (car x))))
	  ((null? x) sum))))

  (test-equal '((6 1 3) (-5 -2))
    (let loop ((numbers '(3 -2 1 6 -5))
	       (nonneg '())
	       (neg '()))
      (cond ((null? numbers) (list nonneg neg))
	    ((>= (car numbers) 0)
	     (loop (cdr numbers)
		   (cons (car numbers) nonneg)
		   neg))
	    ((< (car numbers) 0)
	     (loop (cdr numbers)
		   nonneg
		   (cons (car numbers) neg)))))))

(test-group "4.2.5"
  (define integers
    (letrec ((next
	      (lambda (n)
		(delay (cons n (next (+ n 1)))))))
      (next 0)))
  (define head
    (lambda (stream) (car (force stream))))
  (define tail
    (lambda (stream) (cdr (force stream))))

  (define (stream-filter p? s)
    (delay-force
     (if (null? (force s))
	 (delay '())
	 (let ((h (car (force s)))
	       (t (cdr (force s))))
	   (if (p? h)
	       (delay (cons h (stream-filter p? t)))
	       (stream-filter p? t))))))

  (define count 0)
  (define p
    (delay (begin (set! count (+ count 1))
		  (if (> count x)
		      count
		      (force p)))))
  (define x 5)

  (test-equal 3 (force (delay (+ 1 2))))
  (test-equal '(3 3)
    (let ((p (delay (+ 1 2))))
      (list (force p) (force p))))

  (test-equal 2
    (head (tail (tail integers))))

  (test-equal 5
    (head (tail (tail (stream-filter odd? integers)))))

  (test-assert (promise? p))
  (test-equal 6 (force p))
  (test-assert (promise? p))
  (test-equal 6 (begin (set! x 10)
		       (force p))))

(test-group "4.2.6"
  (define radix
    (make-parameter
     10
     (lambda (x)
       (if (and (exact-integer? x) (<= 2 x 16))
	   x
	   (error "invalid radix")))))

  (define (f n) (number->string n (radix)))

  (test-equal "12" (f 12))
  (test-equal "1100" (parameterize ((radix 2))
		       (f 12)))
  (test-equal "12" (f 12))

  (test-error (parameterize ((radix 0))
		(f 12))))

(test-group "4.2.7"
  (test-equal 42
    (guard (condition
	    ((assq 'a condition) => cdr)
	    ((assq 'b condition)))
      (raise (list (cons 'a 42)))))

  (test-equal '(b . 23)
    (guard (condition
	    ((assq 'a condition) => cdr)
	    ((assq 'b condition)))
      (raise (list (cons 'b 23))))))

(test-group "4.2.8"
  (test-equal '(list 3 4) `(list ,(+ 1 2) 4))
  (test-equal '(list a (quote a))
    (let ((name 'a)) `(list ,name ',name)))
  (test-equal '(a 3 4 5 6 b)
    `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
  (test-equal '((foo 7) . cons)
    `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))
  (test-equal #(10 5 2 4 3 8)
    `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8))
  (test-equal '(list foo bar baz)
    (let ((foo '(foo bar)) (@baz 'baz))
      `(list ,@foo , @baz)))

  (test-equal '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
    `(a `(b ,(+ 1 2) , (foo ,(+ 1 3) d) e) f))
  (test-equal '(a `(b ,x ,'y d) e)
    (let ((name1 'x)
	  (name2 'y))
      `(a `(b ,,name1 ,',name2 d) e)))

  (test-equal '(list 3 4)
    (quasiquote (list (unquote (+ 1 2)) 4)))
  (test-equal '`(list ,(+ 1 2) 4)
    '(quasiquote (list (unquote (+ 1 2)) 4))))

(test-group "4.2.9"
  (define range
    (case-lambda
      ((e) (range 0 e))
      ((b e) (do ((r '() (cons e r))
		  (e (- e 1) (- e 1)))
		 ((< e b) r)))))

  (test-equal '(0 1 2) (range 3))
  (test-equal '(3 4) (range 3 5)))

(test-end)

(test-begin "4.3")

(test-group "4.3.1"
  (test-equal 'now
    (let-syntax ((given-that (syntax-rules ()
			       ((given-that test stmt1 stmt2 ...)
				(if test
				    (begin stmt1
					   stmt2 ...))))))
      (let ((if #t))
	(given-that if (set! if 'now))
	if)))

  (test-equal 'outer
    (let ((x 'outer))
      (let-syntax ((m (syntax-rules () ((m) x))))
	(let ((x 'inner))
	  (m)))))

  (test-equal 7
    (letrec-syntax
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
	       y)))))

(test-group "4.3.2"
  (define-syntax be-like-begin
    (syntax-rules ()
      ((be-like-begin name)
       (define-syntax name
	 (syntax-rules ()
	   ((name expr (... ...))
	    (begin expr (... ...))))))))

  (be-like-begin sequence)

  (test-equal 4 (sequence 1 2 3 4))

  (test-equal 'ok (let ((=> #f))
		    (cond (#t => 'ok)))))

(test-end "4.3")

(test-begin "5")

(test-group "5.5"
  (define-record-type <pare>
    (kons x y)
    pare?
    (x kar set-kar!)
    (y kdr))

  (test-equal #t (pare? (kons 1 2)))
  (test-equal #f (pare? (cons 1 2)))
  (test-equal 1 (kar (kons 1 2)))
  (test-equal 2 (kdr (kons 1 2)))
  (test-equal 3 (let ((k (kons 1 2)))
		  (set-kar! k 3)
		  (kar k))))

(test-end "5")

(test-group "6.12"

  (test-equal 21
    (eval '(* 7 3) (environment '(scheme base))))

  (test-equal 20
    (let ((f (eval '(lambda (f x) (f x x))
		   (null-environment 5))))
      (f + 10)))

  (test-error
   (eval '(define foo 32)
	 (environment '(scheme base)))))

(test-end "R7RS")
