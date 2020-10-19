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

(define-library (scheme base)
  (export *
	  +
	  -
	  ...
	  /
	  <
	  <=
	  =
	  =>
	  >
	  >=
	  _
	  abs
	  and
	  append
	  apply
	  assoc
	  assq
	  assv
	  begin
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
	  case
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
	  cond
	  cond-expand
	  cons
	  current-error-port
	  current-input-port
	  current-output-port
	  define
	  define-record-type
	  define-syntax
	  define-values
	  denominator
	  do
	  dynamic-wind
	  else
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
	  guard
	  if
	  include
	  include-ci
	  inexact
	  inexact?
	  input-port-open?
	  input-port?
	  integer->char
	  integer?
	  lambda
	  lcm
	  length
	  let
	  let*
	  let*-values
	  let-syntax
	  let-values
	  letrec
	  letrec*
	  letrec-syntax
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
	  or
	  output-port-open?
	  output-port?
	  pair?
	  parameterize
	  peek-char
	  peek-u8
	  port?
	  positive?
	  procedure?
	  quasiquote
	  quote
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
	  set!
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
	  syntax-error
	  syntax-rules
	  textual-port?
	  truncate
	  truncate-quotient
          truncate-remainder
          truncate/
	  u8-ready?
	  unless
	  unquote
	  unquote-splicing
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
	  when
	  with-exception-handler
	  write-bytevector
	  write-char
	  write-string
	  write-u8
	  zero?)
  (import (unsyntax $bootstrap)
          (unsyntax derived-forms))
  (include "base.scm"))
