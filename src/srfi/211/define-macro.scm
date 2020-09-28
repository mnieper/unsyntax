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

(define (lisp-transformer proc)
  (lambda (stx)
    (let* ((k (syntax-case stx ()
                ((k . _) #'k)
                (k #'k)))
           (inject (lambda (sym)
                     (datum->syntax k sym)))
           (res (proc (syntax->datum stx))))
      (if (procedure? res)
          (lambda (lookup)
            (inject (res (lambda (sym)
                           (assert (symbol? sym))
                           (lookup (inject sym))))))
          (inject res)))))

(define-syntax define-macro
  (lambda (stx)
    (syntax-case stx ()
      ((define-macro name proc)
       (identifier? #'name)
       #'(define-syntax name
           (lisp-transformer proc)))
      ((define-macro (name . formals) body1 body2 ...)
       (identifier? #'name)
       #'(define-syntax name
           (lisp-transformer
            (lambda (exp)
              (receive formals (apply values (cdr exp))
                body1 body2 ...)))))
      (_
       (syntax-violation #f "ill-formed define-macro definition" stx)))))
