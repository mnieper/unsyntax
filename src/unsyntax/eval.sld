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

(define-library (unsyntax eval)
  (export eval)
  (import (scheme base)
          (scheme lazy))
  (cond-expand
    ((library (unsyntax $bootstrap))
     (import (unsyntax $bootstrap))
     (begin
       (define-syntax runtime-library
         (syntax-rules ()
           ((runtime-library) '(unsyntax stdlibs))))))
    (else
     (import (rename (scheme eval)
                     (eval host-eval)
                     (environment host-environment)))
     (begin
       (define-syntax runtime-library
         (syntax-rules ()
           ((runtime-library) '(unsyntax runtime)))))))
  (begin
    (define *runtime-environment*
      (delay (host-environment (runtime-library))))
    (define (runtime-environment)
      (force *runtime-environment*))
    (define (eval expr)
      (host-eval expr (runtime-environment)))))
