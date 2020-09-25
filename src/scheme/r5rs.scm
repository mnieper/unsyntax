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

(define (null-environment version)
  (unless (and (exact-integer? version) (= 5 version))
    (raise-error 'null-environment
		 "unsupported null environment version ‘~a’" version))
  (environment '(only (scheme base)
		      ...
		      =>
		      _
		      and
		      begin
		      case
		      cond
		      define
		      define-syntax
		      delay
		      do
		      else
		      if
		      lambda
		      let
		      let*
		      let-syntax
		      letrec
		      letrec-syntax
		      or
		      quasiquote
		      quote
		      set!
		      syntax-rules)))

(define (scheme-report-environment version)
  (unless (and (exact-integer? version) (= 5 version))
    (raise-error 'scheme-report-environment
		 "unsupported Scheme report environment version ‘~a’" version))
  (environment '(scheme r5rs)))