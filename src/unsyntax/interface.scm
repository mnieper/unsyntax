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

;;;;;;;;;;;;;;;;;
;; Export Sets ;;
;;;;;;;;;;;;;;;;;

;;; Export sets are datums.

(define (make-export-set marks id* l/p*)
  (cons marks
        (map (lambda (id l/p)
               (define name (identifier-name id))
               (define marks (syntax-object-marks id))
               (cons (cons name marks) l/p))
             id* l/p*)))

(define (export-set-marks export-set) (car export-set))
(define (export-set-exports export-set) (cdr export-set))

(define (export-set-for-each proc export-set)
  (for-each (lambda (p)
              (proc (caar p) (cdar p) (cdr p)))
            (export-set-exports export-set)))

(define (library-export-set lib)
  (define-values (name* l/p*) (hash-table-entries (library-exports lib)))
  (make-export-set '() (map (lambda (name) (make-identifier name '())) name*)
                   l/p*))

;;;;;;;;;;;;;;;;;
;; Import Sets ;;
;;;;;;;;;;;;;;;;;

;;; Import sets are currently implemented as identifier tables.

(define (export-set->import-set k export-set)
  (define xmarks (residual-marks (syntax-object-marks k)
                                 (export-set-marks export-set)))
  (unless xmarks
    (raise-syntax-error k "out-of-context reference of identifier ‘~a’"
                        (identifier-name k)))
  (let ((table (make-identifier-table)))
    (define (add! name marks l/p)
      (define id (make-identifier name (append xmarks marks)))
      (identifier-table-set! table id l/p))
    (export-set-for-each add! export-set)
    table))

(define (residual-marks m1* m2*)
  (define n1 (length m1*))
  (define n2 (length m2*))
  (let f ((n1 n1) (m1* m1*))
    (cond
     ((> n1 n2)
      (cons (car m1*) (f (- n1 1) (cdr m1*))))
     ((marks=? m1* m2*)
      '())
     (else #f))))

(define (make-import-set)
  (make-identifier-table))

(define (import-set-for-each proc import-set)
  (identifier-table-for-each proc import-set))

(define (import-set-auxiliary-syntax k id*)
  (define res (make-identifier-table))
  (define xmarks (syntax-object-marks k))
  (define (add! id)
    (define name (identifier-name id))
    (define marks (syntax-object-marks id))
    (define lbl (auxiliary-syntax-label name))
    (define l/p (make-label/props lbl '()))
    (identifier-table-set! res
                           (make-identifier name (append xmarks marks))
                           l/p))
  (for-each add! id*)
  res)

(define (import-set-only import-set stx*)
  (define res (make-identifier-table))
  (for-each (lambda (stx)
              (define l/p (identifier-table-ref import-set stx))
              (unless l/p
                (raise-syntax-error
                 stx "identifier ‘~a’ not found in original set"
                 (identifier-name stx)))
              (identifier-table-set! res stx l/p))
            stx*)
  res)

(define (import-set-except import-set stx*)
  (define res (make-identifier-table))
  (define excepted-set (make-identifier-table))
  (for-each (lambda (stx)
              (unless (identifier-table-ref import-set stx)
                (raise-syntax-error
                 stx "identifier ‘~a’ not found in original set")
                (identifier-name stx))
              (identifier-table-set! excepted-set stx #t))
            stx*)
  (import-set-for-each (lambda (id l/p)
                         (unless (identifier-table-ref excepted-set id)
                           (identifier-table-set! res id l/p)))
                       import-set)
  res)

(define (import-set-prefix import-set stx)
  (define res (make-identifier-table))
  (define prefix (identifier-name stx))
  (import-set-for-each (lambda (id l/p)
                         (define name (identifier-name id))
                         (define marks (syntax-object-marks id))
                         (define new-name (symbol-append prefix name))
                         (define new-id (make-identifier name marks))
                         (identifier-table-set! res new-id l/p))
                       import-set)
  res)

(define (import-set-rename import-set from-id* to-id*)
  (define res (make-identifier-table))
  (define rename-set (make-identifier-table))
  (for-each (lambda (from-id to-id)
              (unless (identifier-table-ref import-set from-id)
                (raise-syntax-error
                 from-id "identifier ‘~a’ not found in original set"
                 (identifier-name from-id)))
              (identifier-table-update!
               rename-set
               from-id
               values
               (lambda () to-id)
               (lambda (to-id)
                 (raise-syntax-error
                  to-id "identifier ‘~a’ already renamed to ‘~a’"
                  (identifier-name from-id) (identifier-name to-id)))))
            from-id* to-id*)
  (import-set-for-each (lambda (old-id l/p)
                         (define new-id
                           (or (identifier-table-ref rename-set old-id) old-id))
                         (identifier-table-set! res new-id l/p))
                       import-set)
  res)
