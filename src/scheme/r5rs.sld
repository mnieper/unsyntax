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

(define-library (scheme r5rs)
  (export *
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
	  and
	  angle
	  append
	  apply
	  asin
	  assoc
	  assq
	  assv
	  atan
	  begin
	  boolean?
	  caaaar
	  caaadr
	  caaar
	  caadar
	  caaddr
	  caadr
	  caar
	  cadaar
	  cadadr
	  cadar
	  caddar
	  cadddr
	  caddr
	  cadr
	  call-with-current-continuation
	  call-with-input-file
	  call-with-output-file
	  call-with-values
	  car
	  case
	  cdaaar
	  cdaadr
	  cdaar
	  cdadar
	  cdaddr
	  cdadr
	  cdar
	  cddaar
	  cddadr
	  cddar
	  cdddar
	  cddddr
	  cdddr
	  cddr
	  cdr
	  ceiling
	  char->integer
	  char-alphabetic?
	  char-ci<=?
	  char-ci<?
	  char-ci=?
	  char-ci>=?
	  char-ci>?
	  char-downcase
	  char-lower-case?
	  char-numeric?
	  char-ready?
	  char-upcase
	  char-upper-case?
	  char-whitespace?
	  char<=?
	  char<?
	  char=?
	  char>=?
	  char>?
	  char?
	  close-input-port
	  close-output-port
	  complex?
	  cond
	  cons
	  cos
	  current-input-port
	  current-output-port
	  define
	  define-syntax
	  delay
	  denominator
	  display
	  do
	  dynamic-wind
	  eof-object?
	  eq?
	  equal?
	  eqv?
	  eval
	  even?
	  exact->inexact
          exact?
	  exp
	  expt
	  floor
	  for-each
	  force
	  gcd
	  if
	  imag-part
	  inexact->exact
          inexact?
	  input-port?
	  integer->char
	  integer?
	  interaction-environment
	  lambda
	  lcm
	  length
	  let
	  let*
	  let-syntax
	  letrec
	  letrec-syntax
	  list
	  list->string
	  list->vector
	  list-ref
	  list-tail
	  list?
	  load
	  log
	  magnitude
	  make-polar
	  make-rectangular
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
	  null-environment
	  null?
	  number->string
	  number?
	  numerator
	  odd?
	  open-input-file
	  open-output-file
	  or
	  output-port?
	  pair?
	  peek-char
	  positive?
	  procedure?
	  quasiquote
	  quote
	  quotient
	  rational?
	  rationalize
	  read
	  read-char
	  real-part
	  real?
	  remainder
	  reverse
	  round
	  scheme-report-environment
	  set!
	  set-car!
	  set-cdr!
	  sin
	  sqrt
	  string
	  string->list
	  string->number
	  string->symbol
          string-append
	  string-ci<=?
	  string-ci<?
	  string-ci=?
	  string-ci>=?
	  string-ci>?
	  string-copy
	  string-fill!
	  string-length
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
	  symbol?
	  tan
	  truncate
	  values
	  vector
	  vector->list
	  vector-fill!
	  vector-length
	  vector-ref
	  vector-set!
	  vector?
	  with-input-from-file
	  with-output-to-file
	  write
	  write-char
	  zero?)
  (import (rename (scheme base)
		  (exact inexact->exact)
		  (inexact exact->inexact))
	  (scheme char)
	  (scheme complex)
	  (scheme cxr)
	  (scheme eval)
	  (scheme file)
	  (scheme inexact)
	  (scheme lazy)
	  (scheme load)
	  (scheme read)
	  (scheme repl)
	  (scheme write)
	  (unsyntax error))
  (include "r5rs.scm"))
