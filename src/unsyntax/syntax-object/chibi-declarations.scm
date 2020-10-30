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

(import (chibi ast)
        (srfi 125)
        (srfi 128))
(begin
  (define (syntax->datum stx)
    (define table (make-hash-table eq-comparator))
    (let f ((stx stx))
      (let ((datum (if (syntax-object? stx) (syntax-object-expr stx) stx)))
        (cond
         ((pair? datum)
          (or (hash-table-ref/default table datum #f)
              (let ((pair (cons #f #f)))
                (hash-table-set! table datum pair)
                (set-car! pair (f (car datum)))
                (set-cdr! pair (f (cdr datum)))
                pair)))
         ((vector? datum)
          (or (hash-table-ref/default table datum #f)
              (let ((vector (make-vector (vector-length datum))))
                (hash-table-set! table datum vector)
                (do ((i 0 (+ i 1)))
                    ((= i (vector-length datum)))
                  (vector-set! vector i (f (vector-ref datum i))))
                vector)))
         (else
          datum)))))
  (type-printer-set! (type-of (make-syntax-object #f #f #f #f))
                     (lambda (stx wr out)
                       (write-string "#<syntax " out)
                       (wr (syntax->datum stx))
                       (write-char #\space out)
                       (wr (syntax-object-marks stx))
                       (write-string ">" out))))
