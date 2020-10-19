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

(library (rnrs records)
  (export make-record-type-descriptor
          record-type-descriptor?
          record-rtd
          record?
          record-type-name
          record-type-parent
          record-type-uid
          record-type-generative?
          record-type-sealed?
          record-type-opaque?
          record-type-field-names
          record-type-mutable?
          make-record-constructor-descriptor
          record-constructor-descriptor?
          record-constructor
          record-predicate
          record-accessor
          record-mutator)
  (import (scheme base)
          (srfi :1)
          (srfi :2)
          (srfi :125)
          (srfi :128))

  (define *record-type-descriptors* (make-hash-table eq-comparator))

  (define-record-type <record-type-descriptor>
    (%make-record-type-descriptor name parent uid sealed? opaque? fields
                                  total-field-count field-count fields-offset)
    record-type-descriptor?
    (name record-type-name)
    (parent record-type-parent)
    (uid record-type-uid)
    (sealed? record-type-sealed?)
    (opaque? %record-type-opaque?)
    (fields record-type-fields)
    (total-field-count record-type-total-field-count)
    (field-count record-type-field-count)
    (fields-offset record-type-fields-offset))

  (define (make-record-type-descriptor name parent uid sealed? opaque? fields)
    (define (make-default)
      (define fields-offset
        (if parent (record-type-total-field-count parent) 0))
      (define field-count (vector-length fields))
      (%make-record-type-descriptor name parent uid sealed? opaque? fields
                                    (+ fields-offset field-count)
                                    field-count
                                    fields-offset))
    (if uid
        (hash-table-intern! *record-type-descriptors* uid make-default)
        (make-default)))

  (define (record-type-opaque? rtd)
    (or (%record-type-opaque? rtd)
        (and-let* ((parent (record-type-parent rtd)))
          (record-type-opaque? parent))))

  (define (record-type-generative? rtd)
    (not (record-type-uid rtd)))

  (define (record-type-field-names rtd)
    (vector-map second (record-type-fields rtd)))

  (define (record-type-mutable? rtd k)
    (eq? 'mutable (first (vector-ref (record-type-fields rtd) k))))

  (define-record-type <record>
    (make-record rtd fields)
    %record?
    (rtd %record-rtd)
    (fields record-fields))

  (define (record? obj)
    (and (%record? obj)
         (not (record-type-opaque? (%record-rtd obj)))))

  (define (record-rtd r)
    (%record-rtd r))

  (define (record-predicate rtd)
    (lambda (obj)
      (and (%record? obj)
           (let f ((%rtd (%record-rtd obj)))
             (or (eq? %rtd rtd)
                 (and-let* ((%rtd (record-type-parent %rtd)))
                   (f %rtd)))))))

  (define (record-accessor rtd k)
    (lambda (r)
      (vector-ref (record-fields r)
                  (+ k (record-type-fields-offset rtd)))))

  (define (record-mutator rtd k)
    (lambda (r obj)
      (vector-set! (record-fields r)
                   (+ k (record-type-fields-offset rtd))
                   obj)))

  (define-record-type <record-constructor-descriptor>
    (%make-record-constructor-descriptor rtd parent-constructor-descriptor
                                         protocol)
    record-constructor-descriptor?
    (rtd record-constructor-descriptor-rtd)
    (parent-constructor-descriptor record-constructor-descriptor-parent)
    (protocol record-constructor-descriptor-protocol))

  (define (make-record-constructor-descriptor rtd parent-constructor-descriptor
                                              protocol)
    (%make-record-constructor-descriptor rtd parent-constructor-descriptor
                                         protocol))

  (define (record-constructor constructor-descriptor)
    (define rtd (record-constructor-descriptor-rtd constructor-descriptor))
    (define total-field-count (record-type-total-field-count rtd))
    (define (base-constructor . args)
      (define fields (make-vector total-field-count))
      (define r (make-record rtd fields))
      (do ((i 0 (+ i 1))
           (args args (cdr args)))
          ((null? args) r)
        (vector-set! fields i (car args))))
    (let f ((%rcd constructor-descriptor) (%rtd rtd))
      (define protocol (record-constructor-descriptor-protocol %rcd))
      (define parent-rtd (record-type-parent %rtd))
      (define offset (record-type-fields-offset %rtd))
      (if protocol
          (if parent-rtd
              (protocol (lambda parent-args
                          (define constructor
                            (f (record-constructor-descriptor-parent %rcd)
                               parent-rtd))
                          (define r (apply constructor parent-args))
                          (define fields (record-fields r))
                          (lambda args
                            (do ((i offset (+ i 1))
                                 (args args (cdr args)))
                                ((null? args) r)
                              (vector-set! fields i (car args))))))
              (protocol base-constructor))
          base-constructor))))
