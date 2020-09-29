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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntactic environments ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The environment in a syntactic closure is represented by an
;;; identifier.

(define-record-type <syntactic-closure>
  (%make-syntactic-closure env free exp)
  syntactic-closure?
  (env syntactic-closure-environment)
  (free syntactic-closure-free-names)
  (exp syntactic-closure-form))

(define (bound-id=? id1 id2)
  (or (and (symbol? id1) (symbol? id2) (symbol=? id1 id2))
      (and (identifier? id1) (identifier? id2) (bound-identifier=? id1 id2))))

(define (make-syntactic-closure env free exp)
  (cond
   ((identifier? exp)
    (if (member exp free bound-id=?)
        exp
        (add-mark (make-mark) (datum->syntax env exp))))
   ((symbol? exp)
    (if (member exp free bound-id=?)
        exp
        (add-mark (make-mark) (datum->syntax env exp))))
   (else
    (%make-syntactic-closure env free exp))))

(define (close-syntax form env)
  (make-syntactic-closure env '() form))

(define-record-type <capture-syntactic-environment>
  (capture-syntactic-environment proc)
  capture-syntactic-environment?
  (proc capture-syntactic-environment-procedure))
