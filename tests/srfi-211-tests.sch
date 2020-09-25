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

(import (scheme base)
        (srfi 64)
        (srfi 211 syntax-case)
        (srfi 211 er-macro-transformer)
        (srfi 211 ir-macro-transformer))

(test-begin "SRFI 211")

(test-group "ER Macros"
  (define-syntax loop
    (er-macro-transformer
      (lambda (x r c)
        (let ((body (cdr x)))
          `(,(r 'call-with-current-continuation)
            (,(r 'lambda) (exit)
             (,(r 'let) ,(r 'f) () ,@body (,(r 'f)))))))))

  (test-equal 42 (loop (exit 42))))

(test-group "IR Macros"
  (define-syntax loop
    (ir-macro-transformer
     (lambda (expr inject compare)
       (let ((body (cdr expr)))
         `(call-with-current-continuation
           (lambda (,(inject 'exit))
             (let f () ,@body (f))))))))

  (test-equal 42 (loop (exit 42))))

(test-end)
