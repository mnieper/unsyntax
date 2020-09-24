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

;;; XXX: Fix the code below.

(define-primitives! '
  (*
   +
   -
   /
   <
   <=
   =
   >
   >=
   abs
   acos
   append
   apply
   asin
   assoc
   assq
   assv
   atan
   binary-port?
   boolean=?
   boolean?
   bytevector
   bytevector-append
   bytevector-copy
   bytevector-copy!
   bytevector-length
   bytevector-u8-ref
   bytevector-u8-set!
   bytevector?
   caar
   cadr
   call-with-current-continuation
   call-with-port
   call-with-values
   call/cc
   car
   cdar
   cddr
   cdr
   ceiling
   char->integer
   char-ready?
   char<=?
   char<?
   char=?
   char>=?
   char>?
   char?
   close-input-port
   close-output-port
   close-port
   host-command-line
   complex?
   cons
   cos
   current-error-port
   current-input-port
   current-jiffy
   current-output-port
   current-second
   denominator
   display
   dynamic-wind
   emergency-exit
   eof-object
   eof-object?
   eq?
   equal?
   eqv?
   error
   error-object-irritants
   error-object-message
   error-object?
   even?
   exact
   exact-integer-sqrt
   exact-integer?
   exact?
   exit
   exp
   expt
   features
   file-error?
   finite?
   floor
   floor-quotient
   floor-remainder
   floor/
   flush-output-port
   for-each
   force
   gcd
   get-environment-variable
   get-environment-variables
   get-output-bytevector
   get-output-string
   inexact
   inexact?
   infinite?
   input-port-open?
   input-port?
   integer->char
   integer?
   jiffies-per-second
   lcm
   length
   list
   list->string
   list->vector
   list-copy
   list-ref
   list-set!
   list-tail
   list?
   log
   make-bytevector
   make-list
   make-parameter
   make-promise
   make-string
   make-vector
   map
   max
   member
   memq
   memv
   min
   modulo
   ;mutable-environment
   nan?
   negative?
   newline
   not
   null?
   number->string
   number?
   numerator
   odd?
   open-input-bytevector
   open-input-string
   open-output-bytevector
   open-output-string
   output-port-open?
   output-port?
   pair?
   peek-char
   peek-u8
   port?
   positive?
   procedure?
   promise?
   quotient
   raise
   raise-continuable
   rational?
   rationalize
   read
   read-bytevector
   read-bytevector!
   read-char
   read-error?
   read-line
   read-string
   read-u8
   real?
   remainder
   reverse
   round
   sin
   set-car!
   set-cdr!
   sqrt
   square
   string
   string->list
   string->number
   string->symbol
   string->utf8
   string->vector
   string-append
   string-copy
   string-copy!
   string-fill!
   string-for-each
   string-length
   string-map
   string-ref
   string-set!
   string<=?
   string<?
   string=?
   string>=?
   string>?
   string?
   substring
   symbol->string
   symbol=?
   symbol?
   syntax->datum
   tan
   textual-port?
   truncate
   truncate-quotient
   u8-ready?
   utf8->string
   values
   vector
   vector->list
   vector->string
   vector-append
   vector-copy
   vector-copy!
   vector-fill!
   vector-for-each
   vector-length
   vector-map
   vector-ref
   vector-set!
   vector?
   with-exception-handler
   write
   write-bytevector
   write-char
   write-shared
   write-simple
   write-string
   write-u8
   zero?

   ;; Char library
   char-alphabetic?
   char-ci<=?
   char-ci<?
   char-ci=?
   char-ci>=?
   char-ci>?
   char-downcase
   char-foldcase
   char-lower-case?
   char-numeric?
   char-upcase
   char-upper-case?
   char-whitespace?
   digit-value
   string-ci<=?
   string-ci<?
   string-ci=?
   string-ci>=?
   string-ci>?
   string-downcase
   string-foldcase
   string-upcase

   ;; Complex library
   angle
   imag-part
   magnitude
   make-polar
   make-rectangular
   real-part

   ;; CxR library
   caaaar
   caaadr
   caaar
   caadar
   caaddr
   caadr
   cadaar
   cadadr
   cadar
   caddar
   cadddr
   caddr
   cdaaar
   cdaadr
   cdaar
   cdadar
   cdaddr
   cdadr
   cddaar
   cddadr
   cddar
   cdddar
   cddddr
   cdddr

   ;; File library
   call-with-input-file
   call-with-output-file
   delete-file
   file-exists?
   open-binary-input-file
   open-binary-output-file
   open-input-file
   open-output-file
   with-input-from-file
   with-output-to-file

   ;; SRFI 1
   cons*
   every

   ;; SRFI 125
   make-hash-table
   hash-table
   hash-table-unfold
   alist->hash-table
   hash-table?
   hash-table-contains?
   hash-table-empty?
   hash-table=?
   hash-table-mutable?
   hash-table-ref
   hash-table-ref/default
   hash-table-set!
   hash-table-delete!
   hash-table-intern!
   hash-table-update!
   hash-table-update!/default
   hash-table-pop!
   hash-table-clear!
   hash-table-size
   hash-table-keys
   hash-table-values
   hash-table-entries
   hash-table-find
   hash-table-count
   hash-table-map
   hash-table-for-each
   hash-table-map!
   hash-table-map->list
   hash-table-fold
   hash-table-prune!
   hash-table-copy
   hash-table-empty-copy
   hash-table->alist
   hash-table-union!
   hash-table-intersection!
   hash-table-difference!
   hash-table-xor!

   ;; SRFI 128
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
   eq-comparator eqv-comparator equal-comparator

   ;; Gensyms
   gensym
   gensym-count

   ;; Errors
   raise-syntax-error
   syntax-violation

   ;; Syntax objects
   make-syntax-object
   syntax-object?
   syntax-object-expr
   syntax-object-set-expr!
   syntax-object-marks
   syntax-object-substs
   syntax-object-srcloc
   syntax-null?
   syntax-pair?
   syntax-car
   syntax-cdr
   syntax-vector?
   syntax-vector->list
   syntax-length+
   syntax-split-at
   syntax->list

   ;; Substitution environments
   identifier? identifier-name free-identifier=?

   ;; Backend
   host-eval
   host-environment
   host-features))
