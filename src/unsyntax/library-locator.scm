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
  (or (and-let* ((#f)
                 ((pair? name))
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

(define (library-declarations name)
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
                  (generator-for-each yield (lambda ()
                                              (read-syntax p)))))))))
      (library-filenames name)))))

(define (library-declaration? name stx)
  (and-let*
      ((datum (syntax->datum stx))
       ((list? datum))
       ((>= (length datum) 2))
       ((memq (car datum) '(library define-library)))
       ((library-references? name (cadr datum))))))

(define (library-references? ref name)
  ;; TODO: Add library versioning.
  (list= eqv? ref (library-name-normalize name)))

(define (locate-library name)
  (generator-find (lambda (stx)
                    (library-declaration? name stx))
                  (library-declarations name)))
