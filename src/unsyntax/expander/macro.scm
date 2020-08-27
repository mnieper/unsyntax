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

;; (define (make-macro loc transformer) (list loc transformer))
;; (define (macro-location macro) (first macro))
;; (define (macro-transformer macro) (second macro))

(define (expand-transformer stx)
  (receive (expr inv-reqs)
      (parameterize ((invoke-collector (make-library-collector))
                     (visit-collector (make-library-collector)))
        (let ((expr (with-global-store (expand stx))))
          (values expr (invoke-requirements))))
    (for-each (lambda (lib)
                (invoke-library! lib)
                (require-visit! lib))
              inv-reqs)
    expr))

(define (make-transformer-binding type expr)
  (let ((transformer (execute expr)))
    (make-binding (case type
                    ((define-syntax) 'macro)
                    ((define-syntax-parameter) 'macro-parameter))
                  transformer)))

(define (transform transformer stx)
  (cond
   ((variable-transformer? transformer)
    ((variable-transformer-procedure transformer) stx))
   ((procedure? transformer)
    (transformer stx))
   (else
    (raise-error #f "not a transformer"))))

(define transform/macro
  (case-lambda
    ((transformer stx env)
     (transform/macro transformer stx env #f))
    ((transformer stx env id)
     (let* ((loc (syntax-object-srcloc stx))
            (wrap (lambda (e) (datum->syntax #f e loc))))
       (when (and id (not (variable-transformer? transformer)))
         (raise-syntax-error id "not a variable transformer"))
       (let ((marked
              (add-mark (make-mark)
                        (wrap
                         (transform transformer
                                    (add-mark (anti-mark) (wrap stx)))))))
         (if env
             (add-substs env marked)
             marked))))))

(define transform/global-keyword
  (case-lambda
    ((val stx env)
     (transform/global-keyword val stx env #f))
    ((val stx env id)
     (and-let* ((lib (car val)))
       (visit-library! lib))
     (transform/macro (caddr val) stx env id))))
