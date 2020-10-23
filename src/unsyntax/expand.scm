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

;;;;;;;;;;;;;;;;;
;; Code Writer ;;
;;;;;;;;;;;;;;;;;

(define (write-code defs implibs invreqs)
  (for-each (lambda (form)
              (write form)
              (newline))
            (build-code defs implibs invreqs)))

(define (build-code defs implibs invreqs)
  `((import (unsyntax stdlibs))
    (current-features ',(current-features))
    ,@(compile* `((with-error-handler
                   (lambda ()
                     (begin
                       ,@(append (build-libs implibs invreqs)
                                 defs)
                       (if #f #f))))))))

(define (build-libs implibs invreqs)
  (let* ((libvars (make-hash-table eq-comparator))
         (invreqs (hash-table-unfold null? (lambda (invreqs)
                                             (values (car invreqs) #t))
                                     cdr invreqs eq-comparator))
         (invreq? (lambda (lib) (hash-table-ref/default invreqs lib #f))))
    (append-map (lambda (lib)
                  (if (stdlib? lib)
                      '()
                      (build-lib lib invreq? libvars)))
                (get-dependencies implibs))))

(define (build-lib lib invreq? libnames)
  (let ((var (generate-variable "library"))
        (invoker
         ;; TODO: Unify this code with `make-initializer' in
         ;; library-manager.scm.
         (append (library-invoke-code lib)
                 (fold (lambda (label+binding acc)
                         (case (cadr label+binding)
                           ((variable)
                            (let ((loc (caddr label+binding)))
                              (cons
                               `(define-values ,(generate-variable "_")
                                  (set-global! ',loc ,loc))
                               acc)))
                           (else acc)))
                      '() (library-bindings lib)))))
    (hash-table-set! libnames lib var)
    `((define ,var
        (install-library
         ;; Library name
         ',(library-name lib)
         ;; Library version
         ',(library-version lib)
         ;; Visit requirements
         (list ,@(fold (lambda (visreq vars)
                         (let ((var (hash-table-ref libnames visreq)))
                           (if var (cons var vars) vars)))
                       '() (library-visit-requirements lib)))
         ;; Invoke requirements
         (list ,@(fold (lambda (invreq vars)
                         (let ((var (hash-table-ref libnames invreq)))
                           (if var (cons var vars) vars)))
                       '() (library-invoke-requirements lib)))
         ;; Visiter
         (lambda ()
           (begin
             ,@(library-visit-code lib)
             (if #f #f)))
         ;; Invoker
         ,(if (invreq? lib)
              #f
              `(lambda ()
                 (begin
                   ,@invoker
                   (if #f #f))))
         ;; Exports
         ',(exports->alist (library-exports lib))
         ;; Bindings
         ',(library-bindings lib)))
      ,@(if (invreq? lib)
            invoker
            '()))))

(define (stdlib? lib)
  (null? (library-imports lib)))

;;;;;;;;;;;;;;;;;;
;; Main Program ;;
;;;;;;;;;;;;;;;;;;

(define (expand-unsyntax)
  (with-error-handler
   (lambda ()
     (receive (source target)
	 (args-fold (cdr (command-line))
                  (list (option '(#\v "version") #f #f version-etc)
                        (option '(#\h "help") #f #f help)
                        (option '(#\o) #t #f output-file)
                        (option '(#\I) #t #f prepend-path)
                        (option '(#\A) #t #f append-path)
                        (option '(#\D) #t #f add-feature))
                  (lambda (opt name arg . seeds)
                    (raise-error #f
                                 "unrecognized command line option '~a'"
                                 name))
                  (lambda (operand source target)
                    (when source
                      (raise-error #f "more than one input file"))
                    (values operand target))
                  #f #f)
       (unless source
	 (raise-error #f "no input file"))
       (let*-values (((defs implibs invreqs)
		      (expand-program (read-program source)))
		     ((output) (lambda () (write-code defs implibs invreqs))))
	 (cond (target
		(when (file-exists? target)
		  (delete-file target))
		(with-output-to-file target output))
	       (else
		(output))))))))

(define (help opt name arg . seed*)
  (write-string (format "Usage: ~a [options] file
Expand code with Unsyntax.

  -h, --help      Display this help and exit.
  -v, --version   Display version information and exit.
  -I <directory>  Prepend <directory> to library search paths.
  -A <directory>  Append <directory> to library search paths.
  -D <feature>    Add <feature> to list of feature identifiers.
  -o <file>       Place the output into <file>.

Environment:
  UNSYNTAX_LIBRARY_PATH  A colon separated list of library search paths to be
                         used instead of ‘~a’.
"                        (program-invocation-name) (library-vicinity)))
  (emit-bug-reporting-address)
  (exit 0))

(define (output-file opt name arg source target)
  (when target
    (raise-error #f "more than one output file"))
  (values source arg))

(define (prepend-path opt name arg . seed*)
  (current-library-path (cons (sub-vicinity (user-vicinity) arg)
                              (current-library-path)))
  (apply values seed*))

(define (append-path opt name arg . seed*)
  (current-library-path (append! (current-library-path)
                                 (list (sub-vicinity (user-vicinity) arg))))
  (apply values seed*))

(define (add-feature opt name arg . seed*)
  (current-features (cons (string->symbol arg) (current-features)))
  (apply values seed*))
