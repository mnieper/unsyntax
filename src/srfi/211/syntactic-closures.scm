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

(define (rsc-macro-transformer proc)
  (sc-macro-transformer
   (lambda (exp usage-env)
     (capture-syntactic-environment
      (lambda (env)
        (close-syntax (proc exp env) usage-env))))))

(define (sc-identifier? obj)
  (or (symbol? obj) (identifier? obj)))

(define (identifier=? env1 id1 env2 id2)
  (free-identifier=? (close-syntax id1 env1) (close-syntax id2 env2)))

(define (make-synthetic-identifier id)
  (generate-identifier (identifier-name id)))
