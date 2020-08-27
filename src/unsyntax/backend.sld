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

(define-library (unsyntax backend)
  (export
   ;; Expression builders
   build-assignment
   build-begin
   build-body
   build-call
   build-case-lambda
   build-conditional
   build-define-record-type
   build-define-values
   build-delay
   build-delay-force
   build-formals
   build-let-values
   build-letrec
   build-literal
   build-parameterize
   build-primitive
   build-reference
   ;; Execution and compilation
   execute
   compile*)
  (import (scheme base)
          (scheme case-lambda)
          (scheme cxr)
          (srfi 1)
          (srfi 2)
          (srfi 8)
          (srfi 125)
          (srfi 128)
          (unsyntax environment)
          (unsyntax gensym)
          (unsyntax eval)
          (unsyntax store)
          (unsyntax syntax)
          (unsyntax variable))
  (include "backend.scm"))
