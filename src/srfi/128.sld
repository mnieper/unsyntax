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

(define-library (srfi 128)
  (export
   comparator? comparator-ordered? comparator-hashable?
   make-comparator
   make-pair-comparator make-list-comparator make-vector-comparator
   make-eq-comparator make-eqv-comparator make-equal-comparator
   boolean-hash char-hash char-ci-hash
   string-hash string-ci-hash symbol-hash number-hash
   make-default-comparator default-hash comparator-register-default!
   comparator-type-test-predicate comparator-equality-predicate
   comparator-ordering-predicate comparator-hash-function
   comparator-test-type comparator-check-type comparator-hash
   hash-bound hash-salt
   =? <? >? <=? >=?
   comparator-if<=>
   comparator-max comparator-min
   comparator-max-in-list comparator-min-in-list
   default-comparator boolean-comparator real-comparator
   char-comparator char-ci-comparator
   string-comparator string-ci-comparator
   list-comparator vector-comparator
   eq-comparator eqv-comparator equal-comparator)
  (import (unsyntax $bootstrap)))
