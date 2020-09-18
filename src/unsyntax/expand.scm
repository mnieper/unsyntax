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
  (for-each (lambda (e)
	      (write e)
	      (newline))
	    (build-code defs implibs invreqs)))

(define (build-code defs implibs invreqs)
  `((import (unsyntax stdlibs))
    (current-features ',(current-features))
    ,@(build-invokers invreqs)
    ,@defs))

(define (build-invokers libs)
  (append-map (lambda (lib)
                (or (library-invoke-code lib)
                    '()))
              libs))

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
Options:
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
  (current-library-path (cons (sub-vicinity (user-vicinity) arg)
                              (current-library-path)))
  (apply values seed*))

(define (append-path opt name arg . seed*)
  (current-library-path (append! (current-library-path)
                                 (list (sub-vicinity (user-vicinity) arg))))
  (apply values seed*))

(define (add-feature opt name arg . seed*)
  ;; TODO: We have to pass the current features to the compiled
  ;; program.  This can be done by exporting ‘current-features’ from
  ;; ‘(unsyntax stdlibs)’ and setting it.
  (current-features (cons (string->symbol arg) (current-features)))
  (apply values seed*))
