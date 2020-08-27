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

(define-syntax and-let*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ())
       #'#t)
      ((and-let () form form* ...)
       #'(begin form form* ...))
      ((_ ((id expr)))
       #'expr)
      ((_ (id))
       (identifier? #'id)
       #'id)
      ((_ ((expr)))
       #'expr)
      ((_ ((id expr) . claw*) . body)
       #'(let ((id expr))
	   (and id (and-let* claw* . body))))
      ((_ (id . claw*) . body)
       (identifier? #'id)
       #'(and id (and-let* claw* . body)))
      ((_ ((expr) . claw*) . body)
       #'(and expr (and-let* claw* . body)))
      ((_ (claw . claw*) . body)
       (raise-syntax-error #'claw "invalid and-let* claw"))
      (_
       (raise-syntax-error stx "ill-formed and-let* form")))))
