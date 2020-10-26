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

(define-library (unsyntax expander)
  (export
   ;; evaluation
   eval
   eval-syntax
   environment
   mutable-environment
   environment-define!
   ;; library-manager
   find-library
   install-stdlib
   install-library
   install-alias!
   install-auxiliary-syntax!
   create-global-binding!
   rib-import*
   make-library-collector
   visit-collector
   invoke-collector
   visit-requirements
   invoke-requirements
   visit-library!
   invoke-library!
   ;; meta forms
   meta-unbox
   meta-set-box!
   ;; expansion
   expand-top-level
   ;; core-bindings
   core-exports
   ;; cond-expand
   current-features)
  (import (scheme base)
          (scheme case-lambda)
          (scheme cxr)
          (srfi 1)
          (srfi 2)
          (srfi 8)
          (srfi 111)
          (srfi 125)
          (srfi 128)
          (unsyntax assert)
          (unsyntax auxiliary-syntax)
          (unsyntax backend)
          (unsyntax builder)
          (unsyntax error)
          (only (unsyntax features) current-features)
          (unsyntax identifier)
          (unsyntax interface)
          (unsyntax library)
          (unsyntax library-locator)
          (unsyntax read-syntax)
          (unsyntax rib)
          (unsyntax store)
          (unsyntax syntactic-closure)
          (unsyntax syntax)
          (unsyntax transformer)
          (unsyntax variable))
  (include "expander/expand.scm"
           "expander/macro.scm"
           "expander/library-manager.scm"
           "expander/cond-expand.scm"
           "expander/core-bindings.scm"
           "expander/core-syntax.scm"
           "expander/core-forms.scm"
           "expander/core-transformers.scm"
           "expander/syntax-case.scm"
           "expander/auxiliary-syntax.scm"
           "expander/primitives.scm"
           "expander/eval.scm"))
