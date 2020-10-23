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

;;;;;;;;;;;;;;;;;;;;;;
;; Core Environment ;;
;;;;;;;;;;;;;;;;;;;;;;

(define *core-rib* (make-rib))
(define *core-exports* (make-exports))
(define (core-exports) *core-exports*)

(define (define-export! name lbl)
  ;; TODO: Use a commom prefix for these labels.
  (rib-set! *core-rib* (datum->syntax #f name) lbl)
  (exports-set! *core-exports* name (make-label/props lbl '())))

(define (define-core! name b)
  (define-export! name name)
  (bind-core! name b))

;;;;;;;;;;;;;;;;;
;; Core Syntax ;;
;;;;;;;;;;;;;;;;;

(define define-core-syntax!
  (case-lambda
    ((name)
     (define-core-syntax! name name))
    ((name type)
     (define-core! name (make-binding type #f)))))

;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary Syntax ;;
;;;;;;;;;;;;;;;;;;;;;;

(define (define-auxiliary-syntax! name)
  (define-export! name (auxiliary-syntax-label name)))

;;;;;;;;;;;;;;;;
;; Primitives ;;
;;;;;;;;;;;;;;;;

(define (define-primitive! name)
  (define-core! name (make-binding 'primitive name)))

(define (define-primitives! names)
  (for-each define-primitive! names))

;;;;;;;;;;;;;;;;
;; Core Forms ;;
;;;;;;;;;;;;;;;;

(define (define-core-form! name expander)
  (define-core! name (make-binding 'core expander)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Core Transformers ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (core-syntax e)
  (add-substs *core-rib* (datum->syntax #f e)))

(define (define-core-transformer! name transformer)
  (define-core! name (make-binding 'macro (list transformer #f))))
