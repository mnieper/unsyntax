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

(define-library (unsyntax expand)
  (export expand-unsyntax)
  (import (scheme base)
	  (scheme file)
	  (scheme process-context)
	  (scheme write)
	  (srfi 1)
	  (srfi 8)
	  (srfi 28)
	  (srfi 37)
	  (srfi 59)
          (srfi 125)
          (srfi 128)
          (unsyntax backend)
	  (unsyntax error)
	  (unsyntax expander)
          (unsyntax library)
	  (unsyntax library-locator)
	  (unsyntax program)
	  (unsyntax program-name)
	  (unsyntax store)
          (unsyntax variable)
	  (unsyntax version-etc))
  (include "expand.scm"))
