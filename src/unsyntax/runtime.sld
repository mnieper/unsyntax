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

(define-library (unsyntax runtime)
  (include-library-declarations "stdlibs/runtime-exports.scm")
  (import (rename (except (scheme base)
                          define-record-type)
                  (features host-features))
          (scheme case-lambda)
          (scheme cxr)
          (rename (scheme eval)
                  (eval host-eval)
                  (environment host-environment))
	  (scheme inexact)
          (scheme lazy)
	  (rename (scheme process-context)
		  (command-line host-command-line))
	  (scheme time)
	  (scheme write)
          (srfi 1)
          (srfi 27)
          (srfi 111)
          (srfi 125)
          (srfi 128)
          (unsyntax auxiliary-syntax)
	  (unsyntax define-record-type)
          (unsyntax error)
          (unsyntax rib)
          (unsyntax expander)
          (unsyntax features)
          (unsyntax identifier)
          (unsyntax store)
          (unsyntax syntax)))
