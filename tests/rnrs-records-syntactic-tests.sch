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

(import (except (scheme base) define-record-type)
        (scheme inexact)
        (rnrs records syntactic)
        (rnrs records inspection)
        (srfi :64))

(test-begin "R6RS Syntactic Records")

(define-record-type (point make-point point?)
  (fields (immutable x point-x)
          (mutable y point-y set-point-y!))
  (nongenerative
    point-4893d957-e00b-11d9-817f-00111175eb9e))

(define-record-type (cpoint make-cpoint cpoint?)
  (parent point)
  (protocol
   (lambda (n)
     (lambda (x y c)
       ((n x y) (color->rgb c)))))
  (fields
   (mutable rgb cpoint-rgb cpoint-rgb-set!)))

(define (color->rgb c)
  (cons 'rgb c))

(define p1 (make-point 1 2))
(define p2 (make-cpoint 3 4 'red))

(test-equal #t (point? p1))
(test-equal #t (point? p2))

(test-equal #f (point? (vector)))
(test-equal #f (point? (cons 'a 'b)))

(test-equal #f (cpoint? p1))
(test-equal #t (cpoint? p2))

(test-equal 1 (point-x p1))
(test-equal 2 (point-y p1))
(test-equal 3 (point-x p2))
(test-equal 4 (point-y p2))

(test-equal '(rgb . red) (cpoint-rgb p2))

(set-point-y! p1 17)

(test-equal 17 (point-y p1))

(test-eq (record-type-descriptor point) (record-rtd p1))

(define-record-type (ex1 make-ex1 ex1?)
  (protocol (lambda (p) (lambda a (p a))))
  (fields (immutable f ex1-f)))

(define ex1-i1 (make-ex1 1 2 3))

(test-equal '(1 2 3) (ex1-f ex1-i1))

(define-record-type (ex2 make-ex2 ex2?)
  (protocol
    (lambda (p) (lambda (a . b) (p a b))))
  (fields (immutable a ex2-a)
          (immutable b ex2-b)))

(define ex2-i1 (make-ex2 1 2 3))

(test-equal 1 (ex2-a ex2-i1))

(test-equal '(2 3) (ex2-b ex2-i1))

(define-record-type (unit-vector
                     make-unit-vector
                     unit-vector?)
  (protocol
   (lambda (p)
     (lambda (x y z)
       (let ((length
               (sqrt (+ (* x x)
                        (* y y)
                        (* z z)))))
         (p (/ x length)
            (/ y length)
            (/ z length))))))
  (fields (immutable x unit-vector-x)
          (immutable y unit-vector-y)
          (immutable z unit-vector-z)))

(define *ex3-instance* #f)

(define-record-type ex3
  (parent cpoint)
  (protocol
   (lambda (n)
     (lambda (x y t)
       (let ((r ((n x y 'red) t)))
         (set! *ex3-instance* r)
         r))))
  (fields
   (mutable thickness))
  (sealed #t) (opaque #t))

(define ex3-i1 (make-ex3 1 2 17))

(test-equal #t (ex3? ex3-i1))

(test-equal '(rgb . red) (cpoint-rgb ex3-i1))

(test-equal 17 (ex3-thickness ex3-i1))

(ex3-thickness-set! ex3-i1 18)

(test-equal 18 (ex3-thickness ex3-i1))

(test-eq ex3-i1 *ex3-instance*)

(test-equal #f (record? ex3-i1))

(test-end)
