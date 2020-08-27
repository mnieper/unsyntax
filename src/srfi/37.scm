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

(define-record-type <option>
  (option names required-arg? optional-arg? option-proc)
  option?
  (names option-names)
  (required-arg? option-required-arg?)
  (optional-arg? option-optional-arg?)
  (option-proc option-processor))

(define (args-fold args options unrecognized-option-proc operand-proc . seed*)
  (define (find-option name)
    (find (lambda (option)
	    (find (lambda (n)
		    (equal? name n))
		  (option-names option)))
	  options))
  (define (scan-operands operands seed*)
    (if (null? operands)
	(apply values seed*)
	(receive seed* (apply operand-proc (car operands) seed*)
	    (scan-operands (cdr operands) seed*))))
  (let scan-args ((args args) (seed* seed*))
    (define (scan-short-options index shorts args seed*)
      (if (= index (string-length shorts))
	  (scan-args args seed*)
	  (let* ((name
		  (string-ref shorts index))
		 (option
		  (or (find-option name)
		      (option (list name)
			      #f
			      #f
			      unrecognized-option-proc))))
	    (cond
	     ((and (< (+ index 1) (string-length shorts))
		   (or (option-required-arg? option)
		       (option-optional-arg? option)))
	      (receive seed* (apply (option-processor option)
				      option
				      name
				      (string-copy shorts (+ index 1))
				      seed*)
		  (scan-args args seed*)))
	     ((and (option-required-arg? option)
		   (pair? args))
	      (receive seed* (apply (option-processor option)
				      option
				      name
				      (car args)
				      seed*)
		  (scan-args (cdr args) seed*)))
	     (else
	      (receive seed* (apply (option-processor option)
				      option
				      name
				      #f
				      seed*)
		  (scan-short-options (+ index 1) shorts args seed*)))))))
    (if (null? args)
	(apply values seed*)
	(let ((arg (car args))
	      (args (cdr args)))
	  (cond
	   ((string=? "--" arg)
	    (scan-operands args seed*))
	   ((and (> (string-length arg) 4)
		 (char=? #\- (string-ref arg 0))
		 (char=? #\- (string-ref arg 1))
		 (not (char=? #\= (string-ref arg 2)))
		 (let loop ((index 3))
		   (cond
		    ((= index (string-length arg))
		     #f)
		    ((char=? #\= (string-ref arg index))
		     index)
		    (else
		     (loop (+ index 1))))))
	    => (lambda (index)
		 (let*-values
		     (((name)
		       (string-copy arg 2 index))
		      ((option-arg)
		       (string-copy arg (+ index 1)))
		      ((option)
		       (or (find-option name)
			   (option (list name)
				   #t
				   #f
				   unrecognized-option-proc)))
		      (seed*
		       (apply (option-processor option)
			      option
			      name
			      option-arg
			      seed*)))
		   (scan-args args seed*))))
	   ((and (> (string-length arg) 3)
		 (char=? #\- (string-ref arg 0))
		 (char=? #\- (string-ref arg 1)))
	    (let*
		((name
		  (string-copy arg 2))
		 (option
		  (or (find-option name)
		      (option (list name)
			      #f
			      #f
			      unrecognized-option-proc))))
	      (if (and (option-required-arg? option)
		       (pair? args))
		  (receive
		      seed* (apply (option-processor option)
				     option
				     name
				     (car args)
				     seed*)
		    (scan-args (cdr args) seed*))
		  (receive
		      seed* (apply (option-processor option)
				     option
				     name
				     #f
				     seed*)
		    (scan-args args seed*)))))
	   ((and (> (string-length arg) 1)
		 (char=? #\- (string-ref arg 0)))
	    (let ((shorts (string-copy arg 1)))
	      (scan-short-options 0 shorts args seed*)))
	   (else
	    (receive seed* (apply operand-proc arg seed*)
	      (scan-args args seed*))))))))
