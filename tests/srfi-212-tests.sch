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
        (scheme eval)
        (scheme repl)
        (srfi 211 syntax-case)
        (srfi 212)
        (srfi 64))

(eval '(import (srfi 212)) (interaction-environment))

(test-begin "SRFI 212")

(test-equal '(2 1)
  (let* ()
    (define *important-global-variable* '())
    (define (setup!)
      (alias ls *important-global-variable*)
      (set! ls (cons 1 ls))
      (set! ls (cons 2 ls)))
    (setup!)
    *important-global-variable*))

(test-assert (let* ()
               (alias x y)
               (not (free-identifier=? #'x #'y))))

(test-assert (let ((y +))
               (alias x y)
               (let ((y *))
                 (not (free-identifier=? #'x #'y)))))

(test-assert (let ((y +))
               (alias x y)
               (not (bound-identifier=? #'x #'y))))

(test-error
  (test-read-eval-string "(let ((y +))
                            (alias x y)
                            (define y *)
                            (free-identifier=? #'x #'y))"))

(test-equal '(list 3 4)
  (let* ()
    (alias inject unquote)
    `(list (inject (+ 1 2)) 4)))

(test-equal 3
  (let ((y 1))
      (let-syntax ((inject-y
                   (syntax-rules ()
                     ((inject-y x) (alias x y)))))
        (let ((y 2))
          (inject-y x)
          (set! x (* 3 x)))
        y)))

(test-equal '(a pear)
  (syntax-case #'pear ()
    (pvar
     (let* ()
       (alias fruit pvar)
       (syntax->datum #'(a fruit))))))

(test-end)
