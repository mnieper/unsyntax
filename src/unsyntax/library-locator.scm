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
;; Library Path ;;
;;;;;;;;;;;;;;;;;;

(define (split-path path)
  (receive (name name*)
      (let f ((path (string->list path)))
	(if (null? path)
	    (values '() '())
	    (receive (name name*) (f (cdr path))
	      (case (car path)
	       ((#\:)
		(values '() (cons (list->string name) name*)))
	       (else
		=> (lambda (ch)
		     (values (cons ch name) name*)))))))
    (cons (list->string name) name*)))

(define (library-vicinities)
  (cond ((get-environment-variable "UNSYNTAX_LIBRARY_PATH")
         => (lambda (path)
              (map (lambda (name)
                     (sub-vicinity (user-vicinity) name))
                   (split-path path))))
        (else
         (list (library-vicinity)))))

(define *library-path* (delay (library-vicinities)))

(define current-library-path
  (case-lambda
    (() (force *library-path*))
    ((path) (set! *library-path* (make-promise path)))))

(define *library-extensions* '(".unsyntax.sls" ".sls" ".sld"))

(define current-library-extensions
  (case-lambda
    (() *library-extensions*)
    ((lib-exts) (set! *library-extensions* lib-exts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library Name Normalization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (library-name-normalize name)
  (or (and-let* (((pair? name))
                 ((eqv? 'srfi (car name)))
                 ((pair? (cdr name)))
                 ((symbol? (cadr name)))
                 ((symbol-prefix? ': (cadr name)))
                 (number
                  (string->number (symbol->string (symbol-drop (cadr name) 1))))
                 ((exact-integer? number))
                 ((not (negative? number)))
                 ((cons* 'srfi number (cddr name)))))
      name))

(define (library-name-unnormalize name)
  (or (and-let*
          (((pair? name))
           ((eqv? 'srfi (car name)))
           ((pair? (cdr name)))
           ((exact-integer? (cadr name)))
           ((not (negative? (cadr name))))
           ((list name
                  (cons* 'srfi
                         (string->symbol
                          (string-append ":" (number->string (cadr name))))
                         (cddr name))))))
      (list name)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Library Filenames ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define (library-name-part->string part)
  ((if (number? part) number->string symbol->string) part))

(define (library-name->filename vic name ext)
  (let f ((vic vic) (name name))
    (if (null? (cdr name))
        (in-vicinity vic (string-append (library-name-part->string (car name))
                                        ext))
        (f (sub-vicinity vic (library-name-part->string (car name)))
           (cdr name)))))

(define (library-filenames name)
  (let ((lib-exts (current-library-extensions)))
    (gflatten
     (gmap (lambda (vic)
             (append-map (lambda (ext)
                           (map (lambda (name)
                                  (library-name->filename vic name ext))
                                (library-name-unnormalize name)))
                         lib-exts))
           (list->generator (current-library-path))))))

;;;;;;;;;;;;;;;;;;;;;
;; Library Locator ;;
;;;;;;;;;;;;;;;;;;;;;

(define (library-definitions name)
  (make-coroutine-generator
   (lambda (yield)
     (generator-for-each
      (lambda (filename)
	(guard (exc
		((file-error? exc)
		 (raise-error #f "~a: error reading file" filename)))
	  (when (file-exists? filename)
	    (call-with-input-file filename
	      (lambda (in)
                (let ((p (make-source-port in #f filename)))
                  (generator-for-each
                   yield
                   (lambda ()
                     (let ((stx (read-syntax p)))
                       (if (eof-object? stx)
                           stx
                           (receive (name version decls)
                               (parse-library-definition stx)
                             (list stx name version decls))))))))))))
      (library-filenames name)))))

(define (library-definition? name pred def)
  (and (list= eqv? name (cadr def))
       (pred (caddr def))))

(define (locate-library name pred)
  (generator-find (lambda (def)
                    (library-definition? name pred def))
                  (library-definitions name)))

;;;;;;;;;;;;;
;; Parsers ;;
;;;;;;;;;;;;;

(define (parse-library-definition stx)
  (let ((fail (lambda ()
                (raise-syntax-error stx "invalid library definition")))
        (form (syntax->list stx)))
    (case (and form
               (<= 2 (length form))
               (identifier? (car form))
               (identifier-name (car form)))
      ((define-library)
       (receive (name version) (parse-library-name (cadr form))
         (values name version (cddr form))))
      ((library)
       (receive (name version) (parse-library-name (cadr form))
         (unless (and-let*
                     (((<= 4 (length form)))
                      (export-decl (syntax->list (caddr form)))
                      ((pair? export-decl))
                      ((identifier? (car export-decl)))
                      ((eq? 'export (identifier-name (car export-decl))))
                      (import-decl (syntax->list (cadddr form)))
                      ((pair? import-decl))
                      ((identifier? (car import-decl)))
                      ((eq? 'import (identifier-name (car import-decl))))))
           (fail))
         (values name version
                 `(,(caddr form)
                   ,(cadddr form)
                   ,(datum->syntax #f
                                   `(begin . ,(cddddr form))
                                   (syntax-object-srcloc stx))))))
      (else
       (fail)))))

(define (parse-library-name stx)
  (let ((form (syntax->list stx)))
    (unless form
      (raise-syntax-error stx "ill-formed library name"))
    (let ((parts (reverse form)))
      (receive (parts version)
          (if (and (pair? parts)
                   (syntax-pair? (car parts)))
              (values (reverse (cdr parts)) (car parts))
              (values form '()))
        (values (library-name-normalize
                 (map-in-order parse-library-name-part parts))
                (parse-library-version version))))))

(define (parse-library-name-part part)
  (let ((e (syntax-object-expr part)))
    (unless (library-name-part? e)
      (raise-syntax-error part "invalid library name part"))
    e))

(define (library-name-part? obj)
  (or (symbol? obj)
      (and (exact-integer? obj)
           (not (negative? obj)))))

(define (parse-library-version stx)
  (let ((form (syntax->list stx)))
    (unless form
      (raise-syntax-error stx "ill-formed library version"))
    (map-in-order parse-sub-version form)))

(define (parse-version-reference stx)
  (let ((form (syntax->list stx))
        (fail
         (lambda ()
           (raise-syntax-error stx "ill-formed version reference"))))
    (unless form (fail))
    (let ((op (and (pair? form)
                   (identifier? (car form))
                   (identifier-name (car form)))))
      (case op
        ((and)
         (let ((preds
                (map-in-order parse-version-reference (cdr form))))
           (lambda (ver)
             (every (lambda (pred) (pred ver)) preds))))
        ((or)
         (let ((preds
                (map-in-order parse-version-reference (cdr form))))
           (lambda (ver)
             (any (lambda (pred) (pred ver)) preds))))
        ((not)
         (unless (= 2 (length form)) (fail))
         (let ((pred (parse-version-reference (cadr form))))
           (lambda (ver)
             (not (pred ver)))))
        (else
         (let* ((preds (map-in-order parse-sub-version-reference form))
                (n (length preds)))
           (lambda (ver)
             (and (<= n (length ver))
                  (every (lambda (pred sub-ver)
                           (pred sub-ver))
                         preds ver)))))))))

(define (parse-sub-version-reference stx)
  (let ((e (syntax-object-expr stx)))
    (if (and (exact-integer? e) (not (negative? e)))
        (lambda (sub-ver)
          (= e sub-ver))
        (let ((form (syntax->list stx))
              (fail
               (lambda ()
                 (raise-syntax-error stx "ill-formed sub-version reference"))))
          (unless form (fail))
          (let ((op (and (pair? form)
                         (identifier? (car form))
                         (identifier-name (car form)))))
            (case op
              ((>=)
               (unless (= 2 (length form)) (fail))
               (let ((e (parse-sub-version (cadr form))))
                 (lambda (sub-ver)
                   (>= sub-ver e))))
              ((<=)
               (unless (= 2 (length form)) (fail))
               (let ((e (parse-sub-version (cadr form))))
                 (lambda (sub-ver)
                   (<= sub-ver e))))
              ((and)
               (let ((preds
                      (map-in-order parse-sub-version-reference (cdr form))))
                 (lambda (sub-ver)
                   (every (lambda (pred) (pred sub-ver)) preds))))
              ((or)
               (let ((preds
                      (map-in-order parse-sub-version-reference (cdr form))))
                 (lambda (sub-ver)
                   (any (lambda (pred) (pred sub-ver)) preds))))
              ((not)
               (unless (= 2 (length form)) (fail))
               (let ((pred (parse-sub-version-reference (cadr form))))
                 (lambda (sub-ver)
                   (not (pred sub-ver)))))
              (else (fail))))))))

(define (parse-sub-version stx)
  (let ((e (syntax-object-expr stx)))
    (unless (and (exact-integer? e) (not (negative? e)))
      (raise-syntax-error stx "invalid library sub-version" stx))
    e))
