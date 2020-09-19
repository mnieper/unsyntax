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

(include-library-declarations "stdlibs/runtime-exports.scm")
(export current-features expand-unsyntax unsyntax-scheme install-library
        environment syntax-object)
(import (rename (except (scheme base) define-record-type)
                (features host-features))
        (scheme case-lambda)
        (scheme char)
        (scheme complex)
        (scheme cxr)
        (rename (scheme eval)
                (eval host-eval)
                (environment host-environment))
        (scheme file)
        (scheme inexact)
        (scheme lazy)
        (rename (scheme process-context)
		(command-line host-command-line))
        (scheme write)
        (srfi 125)
        (srfi 128)
        (unsyntax define-record-type)
        (unsyntax syntax-object)
        (unsyntax gensym))
(begin
  (define-syntax environment
    (syntax-rules ()
      ((environment . substs)
       (cons #f 'substs))))

  (define-syntax syntax-object
    (syntax-rules ()
      ((syntax-object expr marks substs)
       (make-syntax-object expr
                           'marks
                           (list . substs)
                           #f)))))
