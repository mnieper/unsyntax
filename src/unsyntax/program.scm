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

;;;;;;;;;;;;;;;;;;;;;;;
;; Program Execution ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (run stx)
  (let-values (((defs implibs invreqs) (expand-program stx))
	       ((loc) (syntax-object-srcloc stx)))
    (for-each invoke-library! invreqs)
    (execute (build-body loc defs
			 (build-primitive-call loc 'exit
					       (list (build-literal loc 0)))))))

;;;;;;;;;;;;;;;;;;;;;;
;; Program Expander ;;
;;;;;;;;;;;;;;;;;;;;;;

(define (expand-program stx)
  (let*-values (((import-decl body) (parse-program stx))
		((import-sets) (parse-import-declaration import-decl))
		((import-env) (make-environment))
		((imported-libs) (environment-import* import-env import-sets)))
    (parameterize ((visit-collector (make-library-collector))
		   (invoke-collector (make-library-collector)))
      (expand-top-level body
			import-env
			(lambda (stxdefs defs env)
			  (values defs
				  imported-libs
				  (invoke-requirements)))))))

(define (parse-program stx)
  (let ((body (syntax->list stx)))
    (unless body
      (raise-syntax-error stx "ill-formed program"))
    (when (null? body)
      (raise-syntax-error stx "missing import declaration"))
    (values (car body) (syntax-cdr stx))))

;;;;;;;;;;;;;;;;;;;;
;; Program Reader ;;
;;;;;;;;;;;;;;;;;;;;

(define (read-program source)
  (datum->syntax #f (read-source source) (source-location source #f #f)))
