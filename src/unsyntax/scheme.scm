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

(define (unsyntax-scheme)
  (with-error-handler
   (lambda ()
     (receive (source arg*)
         (args-fold (cdr (command-line))
                    (list (option '(#\v "version") #f #f version-etc)
                          (option '(#\h "help") #f #f help)
                          (option '(#\I) #t #f prepend-path)
                          (option '(#\A) #t #f append-path)
                          (option '(#\D) #t #f add-feature))
                    (lambda (opt name arg . seeds)
                      (raise-error
                       #f "unrecognized command line option ‘~a’" name))
                    (lambda (operand source arg*)
		      (if source
			  (values source (cons operand arg*))
			  (values operand '())))
		    #f '())
       (if source
	   (let ((stx (read-program source)))
	     (current-command-line (cons source (reverse! arg*)))
	     (run stx))
	   (repl))))))

(define (help opt name arg . seeds)
  (write-string (format "Usage: ~a [options] [--] [program [arg]...]
Evaluate code with Unsyntax, interactively or from a program.

  -h, --help      Display this help and exit.
  -v, --version   Display version information and exit.
  -I <directory>  Prepend <directory> to library search paths.
  -A <directory>  Append <directory> to library search paths.
  -D <feature>    Add <feature> to list of feature identifiers.

Environment:
  UNSYNTAX_LIBRARY_PATH  A colon separated list of library search paths to be
                         used instead of ‘~a’.
"                        (program-invocation-name) (library-vicinity)))
  (emit-bug-reporting-address)
  (exit 0))

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

;;;;;;;;;;;;;;
;; The REPL ;;
;;;;;;;;;;;;;;

(define (undefined? obj)
  (eqv? (if #f #f) obj))

(define (readline)
  (write-string "#,> ")
  (read))

(define (repl)
  (emit-version)
  (newline)
  (do () (#f)
    (with-interactive-error-handler
     (lambda ()
       (let ((expr (readline)))
         (when (eof-object? expr)
           (exit 0))
         (call-with-values
             (lambda ()
               (eval expr (interaction-environment)))
           (case-lambda
             ((res)
              (unless (undefined? res)
                (write res)
                (newline)))
             (res*
              (do ((res* res* (cdr res*))
                   (sep "" " "))
                  ((null? res*))
                (write-string sep)
                (write (car res*)))
              (newline)))))))))
