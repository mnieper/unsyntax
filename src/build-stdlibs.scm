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

(import (scheme base)
        (scheme cxr)
        (scheme file)
        (scheme read)
        (scheme write)
        (scheme process-context)
        (srfi 1)
        (srfi 8)
        (srfi 28)
        (srfi 37)
        (srfi 59)
        (srfi 125)
        (srfi 128)
        (srfi 158)
        (unsyntax backend)
        (unsyntax bootstrap)
        (unsyntax environment)
        (unsyntax error)
        (unsyntax expander)
        (unsyntax gensym)
        (unsyntax program-name)
        (unsyntax library)
        (unsyntax library-locator)
        (unsyntax store)
        (unsyntax variable)
        (unsyntax version-etc))

(define (help opt name arg . seed*)
  (write-string (format "Usage: ~a [options] file
Bootstrap Unsyntax and build its standard library.

  -h, --help      Display this help and exit.
  -v, --version   Display version information and exit.
  -I <directory>  Prepend <directory> to library search paths.
  -A <directory>  Append <directory> to library search paths.
  -D <feature>    Add <feature> to list of feature identifiers.
  -o <file>       Place the output into <file>.
"                        (program-invocation-name)))
  (emit-bug-reporting-address)
  (exit 0))

(define (output-file opt name arg source target)
  (when target
    (raise-error #f "more than one output file"))
  (values source arg))

(define (prepend-path opt name arg . seed*)
  (define vic (sub-vicinity (user-vicinity) arg))
  (current-library-path (cons vic (current-library-path)))
  (apply values seed*))

(define (append-path opt name arg . seed*)
  (define vic (sub-vicinity (user-vicinity) arg))
  (current-library-path (append! (current-library-path) (list vic)))
  (apply values seed*))

(define (add-feature opt name arg . seed*)
  (current-features (cons (string->symbol arg) (current-features)))
  (apply values seed*))

(define (read* port)
  (generator->list (lambda () (read port))))

(define (write-stdlibs libs core-lib aliases auxlibs)
  (write (build-stdlibs libs core-lib aliases auxlibs))
  (newline))

(define (build-stdlibs libs core-lib aliases auxlibs)
  `(define-library (unsyntax stdlibs)
     (export ,@(build-exports (current-globals)))
     (include-library-declarations "stdlibs/runtime.scm")
     (begin (gensym-count ,(gensym-count))
            ,@(compile* (append (build-defs* (current-libraries))
                                (build-symbol-defs core-lib)
                                (build-auxlibs auxlibs)
                                (build-installers libs aliases)
                                (build-globals (current-libraries))
                                (build-visiters (current-libraries)))))))

(define (build-exports globals)
  (fold-right (lambda (entry locs)
                (let ((b (cdr entry)))
                  (case (binding-type b)
                    ((global-variable)
                     (cons (cadr (binding-value b))
                           locs))
                    (else locs))))
              '() globals))

(define (build-defs* libs)
  (append-map build-defs libs))

(define (build-defs lib)
  (library-invoke-code lib))

(define (build-symbol-defs core-lib)
  (exports-map->list (lambda (name lbl)
                       `(define ,name ,(cadr (binding-value (lookup lbl)))))
                     (library-exports core-lib)))

(define (build-auxlibs auxlibs)
  (map (lambda (auxlib)
         `(install-auxiliary-syntax! ',auxlib))
       auxlibs))

(define (build-installers libs aliases)
  (let* ((libvars (make-hash-table equal-comparator))
         (installers
          (map (lambda (lib)
                 (let ((var (generate-variable "library")))
                   (hash-table-set! libvars (library-name lib) var)
                   (build-installer lib var)))
               libs)))
    (append installers
            (map (lambda (alias)
                   `(install-alias! ,(hash-table-ref libvars (car alias))
                                    ',(cdr alias)))
                 aliases))))

(define (build-globals libs)
  (append-map (lambda (lib)
                (append
                 (map (lambda (entry)
                        `(bind-global-variable! ',(car entry) ',(cdr entry)))
                      (library-variables lib))
                 (map (lambda (entry)
                        `(bind-global-keyword! ',(car entry) ',(cdr entry)))
                      (library-keywords lib))))
              libs))

(define (build-visiters libs)
  (append-map library-visit-code (current-libraries)))

(define (build-installer lib var)
  `(define ,var
     (install-stdlib ',(library-name lib)
                     ',(exports->alist (library-exports lib))
                     ',(library-keywords lib)
                     ',(library-variables lib))))

(define (find-library/die name)
  (or (find-library name) (raise-error #f "library ‘~a’ not found" name)))

(define (parse-stdlibs decls)
  (let f ((decls decls) (stdlibs '()) (aliases '()) (auxlibs '()))
    (if (null? decls)
        (values stdlibs aliases auxlibs)
        (let ((decl (car decls)))
          (cond
           ((alias decl)
            => (lambda (alias)
                 (f (cdr decls) stdlibs (cons alias aliases) auxlibs)))
           ((auxiliary-syntax decl)
            => (lambda (name)
                 (f (cdr decls) stdlibs aliases (cons name auxlibs))))
           (else
            (f (cdr decls) (cons decl stdlibs) aliases auxlibs)))))))

(define (alias decl)
  (and (pair? (cdr decl))
       (pair? (cadr decl))
       (eq? 'alias (car decl))
       (cons (cadr decl) (caddr decl))))

(define (auxiliary-syntax decl)
  (and (pair? (cdr decl))
       (pair? (cadr decl))
       (eq? 'auxiliary-syntax (car decl))
       (cadr decl)))

;;;;;;;;;;;;;;;;;;;;
;; Initialization ;;
;;;;;;;;;;;;;;;;;;;;

(install-bootstrap-library!)

;;;;;;;;;;;;;;;;;;
;; Main Program ;;
;;;;;;;;;;;;;;;;;;

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
                  (lambda (opt name arg . seed*)
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
     (let*-values (((core-lib) (find-library/die '(unsyntax core-procedures)))
                   ((stdlibs aliases auxlibs)
                    (parse-stdlibs (call-with-input-file source read*)))
                   ((libs) (map find-library/die stdlibs))
                   ((output) (lambda ()
                               (write-stdlibs libs core-lib aliases auxlibs))))
       (cond (target
              (when (file-exists? target)
                (delete-file target))
              (with-output-to-file target output))
             (else
              (output)))))))
