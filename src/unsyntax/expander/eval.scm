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

;;;;;;;;;;;;;;;;;;
;; Environments ;;
;;;;;;;;;;;;;;;;;;

(define (environment . import-sets)
  (let ((env (make-environment)))
    (%environment-import env import-sets)
    env))

(define (mutable-environment . import-sets)
  (let ((env (make-mutable-environment)))
    (environment-set! env (datum->syntax #f 'import) 'import)
    (%environment-import env import-sets)
    env))

(define (%environment-import env import-sets)
  (for-each (lambda (import-set)
              (environment-import env (datum->syntax #f import-set)))
            import-sets))

(define (environment-define! env name val)
  (unless (environment-mutable? env)
    (raise-error 'environment-define
                 "trying to define ‘~a’ in immutable environment" name))
  (mutable-set! env (datum->syntax #f name) val))

(define (mutable-ref env id)
  (or (environment-ref env id)
      (let ((var (genvar id))
            (lbl (genlbl id)))
        (environment-set! env id lbl)
        (bind-global! lbl (make-binding 'mutable-variable var))
        (set-global! var (box (if #f #f)))
        lbl)))

(define (mutable-set! env id val)
  (let* ((lbl (mutable-ref env id))
         (b (lookup lbl)))
    (if (eq? 'mutable-variable (binding-type b))
        (set-box! (ref-global (binding-value b)) val)
        (let ((var (genvar id)))
          (bind-global! lbl (make-binding 'mutable-variable var))
          (set-global! var (box val))))))

;;;;;;;;;;;;;;;;
;; Evaluation ;;
;;;;;;;;;;;;;;;;

(define (eval expr env)
  (eval-syntax (datum->syntax #f expr) env))

(define (eval-syntax stx env)
  (let ((wrapped (add-substs env stx)))
    (if (environment-mutable? env)
        (eval-repl wrapped env)
        (eval-expression wrapped))))

(define (eval-expression stx)
  (receive (expr invreqs) (expand-expression stx)
    (for-each invoke-library! invreqs)
    (execute expr)))

(define (expand-expression stx)
  (parameterize ((invoke-collector (make-library-collector)))
    (let ((expr (expand stx)))
      (values expr (invoke-requirements)))))

(define (eval-repl stx env)
  (parameterize ((current-global-resolver (lambda (id) (mutable-ref env id))))
    (let f ((body (list stx)) (vals (list (if #f #f))))
      (if (null? body)
          (apply values vals)
          (receive (stx type val) (syntax-type (car body) env)
            (case type
              ((begin)
               (f (append (parse-begin stx #f) (cdr body)) vals))
              ;; FIXME
              ((define-auxiliary-syntax)
               (error "not implemented"))
              ;; FIXME
              ((define-record-type)
               (error "not implemented"))
              ;; FIXME
              ((define-values)
               (error "not implemented"))
              ;; FIXME
              ((define-syntax)
               (error "not implemented"))
              ((import)
               (environment-import* env (parse-import-declaration stx))
               (f (cdr body) (list (if #f #f))))
              ;; FIXME
              ((let-syntax letrec-syntax)
               (error "not implemented"))
              ;; FIXME
              ((with-ellipsis)
               (error "not implemented"))
              (else
               (receive vals (eval-expression stx)
                 (f (cdr body) vals)))))))))

(define (parse-import-declaration stx)
  (let ((form (syntax->list stx)))
    (unless (and (pair? form)
		 (identifier? (car form))
		 (eq? 'import (identifier-name (car form))))
      (raise-syntax-error stx "ill-formed import declaration"))
    (cdr form)))
