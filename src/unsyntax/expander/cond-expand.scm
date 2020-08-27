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

(define (eval-cond-expand stx)
  (receive (req* res*) (parse-cond-expand stx)
    (let f ((req (car req*)) (req* (cdr req*))
            (res (car res*)) (res* (cdr res*)))
      (cond
       ((eval-feature-requirement req (null? req*))
        res)
       ((null? req*)
        (raise-syntax-error stx "no cond-expand feature requirement fulfilled"))
       (else
        (f (car req*) (cdr req*)
           (car res*) (cdr res*)))))))

(define (eval-feature-requirement req last?)
  (let ((f
         (lambda (req) (eval-feature-requirement req #f)))
        (fail
         (lambda ()
           (raise-syntax-error req
                               "ill-formed cond-expand feature requirement"))))
    (if (identifier? req)
        (let ((name (identifier-name req)))
          (cond
           ((eq? 'else name)
            (or last? (fail)))
           ((eq? 'unsyntax name))
           (else
            (memq name (current-features)))))
        (let ((form (syntax->list req)))
          (unless (and form
                       (<= 1 (length form))
                       (identifier? (car form))) (fail))
          (case (identifier-name (car form))
            ((and)
             (every values (map f (cdr form))))
            ((or)
             (any values (map f (cdr form))))
            ((not)
             (unless (= 2 (length form)) (fail))
             (not (f (cadr form))))
            ((library)
             (unless (= 2 (length form)) (fail))
             (library-present? (cadr form)))
            (else (fail)))))))

(define (parse-cond-expand stx)
  (let ((fail (lambda ()
                (raise-syntax-error stx "ill-formed cond-expand form")))
        (form (syntax->list stx)))
    (unless (and form (<= 2 (length form)))
      (fail))
    (let f ((stx* (cdr form)))
      (if (null? stx*)
          (values '() '())
          (receive (req* e*) (f (cdr stx*))
            (define form (syntax->list (car stx*)))
            (unless (and form (<= 1 (length form))) (fail))
            (values (cons (car form) req*)
                    (cons (cdr form) e*)))))))
