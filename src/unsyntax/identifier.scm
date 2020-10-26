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

;;;;;;;;;;;;;;;;;;;;;;;
;; Identifier Tables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (make-identifier-table) (make-hash-table eq-comparator))

(define identifier-table? hash-table?)

(define (identifier-table-cell identifier-table id)
  (define name (identifier-name id))
  (define marks (syntax-object-marks id))
  (define (failure) #f)
  (define (success alist) (assoc marks alist marks=?))
  (hash-table-ref identifier-table name failure success))

(define identifier-table-cell-ref cdr)

(define identifier-table-cell-set! set-cdr!)

(define (identifier-table-ref identifier-table id)
  (and-let* ((cell (identifier-table-cell identifier-table id)))
    (identifier-table-cell-ref cell)))

(define (identifier-table-update! identifier-table id update failure success)
  (define name (identifier-name id))
  (define marks (syntax-object-marks id))
  (define (updater alist)
    (cond ((assoc marks alist marks=?)
           => (lambda (p)
                (set-cdr! p (update (success (cdr p))))
                alist))
          (else
           (alist-cons marks (update (failure)) alist))))
  (hash-table-update!/default identifier-table name updater '()))

(define (identifier-table-set! identifier-table id val)
  (identifier-table-update! identifier-table
			    id
			    values
			    (lambda ()
			      val)
			    (lambda (prev-val)
			      val)))

(define (identifier-table-for-each proc table)
  (hash-table-for-each
   (lambda (name alist)
     (for-each (lambda (p)
                 (proc (make-identifier name (car p)) (cdr p)))
               alist))
   table))

(define (identifier-table->alist table)
  (hash-table-fold
   (lambda (name alist datum)
     (fold (lambda (p datum)
             (define marks (car p))
             (define val (cdr p))
             (alist-cons (make-identifier name marks) val datum))
           datum alist))
   '() table))

(define (identifier-table->datum table filter)
  (hash-table-fold
   (lambda (name alist datum)
     (fold (lambda (p datum)
             (cond
              ((filter (cdr p)) =>
               (lambda (val)
                 (define marks (car p))
                 (alist-cons (cons name marks) val datum)))
              (else datum)))
	   datum alist))
   '() table))

(define (datum->identifier-table datum)
  (define res (make-identifier-table))
  (for-each (lambda (p)
              (define name (caar p))
              (define marks (cdar p))
              (define val (cdr p))
              (identifier-table-set! res (make-identifier name marks) val))
            datum)
  res)
