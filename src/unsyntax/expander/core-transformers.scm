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

;;;;;;;;;;;;;;;;;
;; Cond-Expand ;;
;;;;;;;;;;;;;;;;;

(define-core-transformer! 'cond-expand
  (lambda (stx)
    `(,(core-syntax 'begin) ,@(eval-cond-expand stx))))

;;;;;;;;;;;;;
;; Include ;;
;;;;;;;;;;;;;

(define (include-transformer ci?)
  (lambda (stx)
    (let* ((filenames (parse-include stx))
           (k (syntax-car stx))
           (srcloc (syntax-object-srcloc stx)))
      `(,(core-syntax 'begin)
        ,@(append-map (lambda (filename)
                        (datum->syntax
                         k (read-file (datum->syntax #f filename srcloc) ci?)
                         srcloc))
                      filenames)))))

(define-core-transformer! 'include (include-transformer #f))
(define-core-transformer! 'include-ci (include-transformer #t))

(define (parse-include stx)
  (let ((fail
         (lambda ()
           (raise-syntax-error stx "ill-formed include(-ci) form")))
        (form (syntax->list stx)))
    (unless form (fail))
    (let ((filenames (map syntax->datum (cdr form))))
      (unless (every string? filenames) (fail))
      filenames)))
