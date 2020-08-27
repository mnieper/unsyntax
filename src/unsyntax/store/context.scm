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

;;;;;;;;;;;;;;
;; Contexts ;;
;;;;;;;;;;;;;;

(define-record-type <context>
  (make-context libraries globals locations)
  context?
  (libraries context-library-table)
  (globals context-global-store)
  (locations context-locations))

(define (new-context)
  (make-context (make-library-table) (new-store)
                (make-hash-table variable-comparator)))

(define current-context (make-parameter (new-context)))

(define (current-library-table)
  (context-library-table (current-context)))

(define (current-global-store)
  (context-global-store (current-context)))

(define (current-locations) (context-locations (current-context)))

(define current-store (make-parameter (current-global-store)))

(define-syntax with-new-context
  (syntax-rules ()
    ((with-new-context body1 body2 ...)
     (parameterize ((current-context (new-context)))
       (parameterize ((current-store (current-global-store)))
         body1 body2 ...)))))
