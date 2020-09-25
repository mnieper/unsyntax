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

(define (make-identifier name m*)
  (make-syntax-object name m* '() #f))

(define (identifier? stx)
  (and (syntax-object? stx) (symbol? (syntax-object-expr stx))))

(define (identifier-name stx)
  (syntax-object-expr stx))

(define (bound-identifier=? id1 id2)
  (and (symbol=? (syntax-object-expr id1)
                 (syntax-object-expr id2))
       (marks=? (syntax-object-marks id1)
                (syntax-object-marks id2))))

(define generate-identifier
  (case-lambda
    (()
     (generate-identifier (gensym "t")))
    ((sym)
     (make-syntax-object sym (list (make-mark)) '() #f))))
