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

(define-library (unsyntax core-procedures)
  (export
   ;; Libraries
   install-stdlib
   install-library
   install-alias!
   install-auxiliary-syntax!
   create-global-binding!
   set-global!
   set-keyword!
   set-property!

   ;; Syntax-case
   cons*
   fold-right
   free-identifier=?
   raise-syntax-error
   syntax-violation
   syntax-car
   syntax-cdr
   syntax->datum
   syntax->list
   syntax-length+
   syntax-null?
   syntax-pair?
   syntax-split-at
   syntax-vector?
   syntax-vector->list

   ;; Procedures for base macros
   auxiliary-syntax
   every
   identifier?

   ;; Replaced base procedures
   read-error?

   ;; REPL
   unsyntax-scheme

   ;; Compiler
   expand-unsyntax

   ;; Boxes for mutable variables
   unbox
   set-box!

   ;; Meta definitions
   arguments->vector
   meta-unbox
   meta-set-box!

   ;; Base Library
   command-line

   ;; Error reporting in programs
   with-error-handler

   ;; Features for cond-expand and the -D command-line flag.
   current-features
   features)
  (import (srfi 1)
          (srfi 111)
          (unsyntax auxiliary-syntax)
	  (unsyntax command-line)
          (unsyntax rib)
          (rename (unsyntax error)
                  (reader-error? read-error?))
	  (unsyntax expand)
          (unsyntax expander)
          (unsyntax features)
          (unsyntax identifier)
          (unsyntax scheme)
          (unsyntax store)
          (unsyntax syntax)))
