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

(define *interaction-environment*
  (delay
    (let
        ((env
          (mutable-environment
            '(only (unsyntax) import)
            '(only (scheme base)
                   ...
                   =>
                   _
                   and
                   begin
                   case
                   cond
                   cond-expand
                   define
                   define-record-type
                   define-syntax
                   define-values
                   do
                   else
                   guard
                   if
                   include
                   include-ci
                   lambda
                   let
                   let*
                   let*-values
                   let-values
                   let-syntax
                   letrec
                   letrec*
                   letrec-syntax
                   or
                   parameterize
                   quasiquote
                   quote
                   set!
                   syntax-error
                   syntax-rules
                   unless
                   unquote
                   unquote-splicing
                   when))))
      (let-syntax ((define* (syntax-rules ()
                              ((_ i ...)
                               (begin
                                 (environment-define! env 'i i) ...)))))
        (define*
          *
          +
          -
          /
          <
          <=
          =
          >
          >=
          abs
          append
          apply
          assoc
          assq
          assv
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
          complex?
          cons
          current-error-port
          current-input-port
          current-output-port
          denominator
          dynamic-wind
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
          expt
          features
          file-error?
          floor
          floor-quotient
          floor-remainder
          floor/
          flush-output-port
          for-each
          gcd
          get-output-bytevector
          get-output-string
          inexact
          inexact?
          input-port-open?
          input-port?
          integer->char
          integer?
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
          make-bytevector
          make-list
          make-parameter
          make-string
          make-vector
          map
          max
          member
          memq
          memv
          min
          modulo
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
          quotient
          raise
          raise-continuable
          rational?
          rationalize
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
          set-car!
          set-cdr!
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
          write-bytevector
          write-char
          write-string
          write-u8
          zero?)
        env))))

(define (interaction-environment) (force *interaction-environment*))
