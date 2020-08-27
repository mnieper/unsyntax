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

(define-record-type test-runner-type
  (%make-test-runner on-test-begin
		     on-test-end
		     on-group-begin
		     on-group-end
		     on-bad-count
		     on-bad-end-name
		     on-final)
  test-runner?
  (on-test-begin test-runner-on-test-begin test-runner-on-test-begin!)
  (on-test-end test-runner-on-test-end test-runner-on-test-end!)
  (on-group-begin test-runner-on-group-begin test-runner-on-group-begin!)
  (on-group-end test-runner-on-group-end test-runner-on-group-end!)
  (on-bad-count test-runner-on-bad-count test-runner-on-bad-count!)
  (on-bad-end-name test-runner-on-bad-end-name test-runner-on-bad-end-name!)
  (on-final test-runner-on-final test-runner-on-final!)
  (alist test-result-alist test-result-alist!)
  (pass-count test-runner-pass-count test-runner-pass-count!)
  (fail-count test-runner-fail-count test-runner-fail-count!)
  (xpass-count test-runner-xpass-count test-runner-xpass-count!)
  (xfail-count test-runner-xfail-count test-runner-xfail-count!)
  (skip-count test-runner-skip-count test-runner-skip-count!)
  (group-stack test-runner-group-stack test-runner-group-stack!)
  (aux-value test-runner-aux-value test-runner-aux-value!)
  (count-list test-runner-count-list test-runner-count-list!)
  (run-list test-runner-run-list test-runner-run-list!)
  (skip-list test-runner-skip-list test-runner-skip-list!)
  (fail-list test-runner-fail-list test-runner-fail-list!)
  (skip-save test-runner-skip-save test-runner-skip-save!)
  (fail-save test-runner-fail-save test-runner-fail-save!)
  (total-count test-runner-total-count test-runner-total-count!))

(define (test-runner-reset runner)
  (test-result-alist! runner '())
  (test-runner-pass-count! runner 0)
  (test-runner-fail-count! runner 0)
  (test-runner-xpass-count! runner 0)
  (test-runner-xfail-count! runner 0)
  (test-runner-skip-count! runner 0)
  (test-runner-group-stack! runner '())
  (test-runner-count-list! runner '())
  (test-runner-run-list! runner #t)
  (test-runner-skip-list! runner '())
  (test-runner-fail-list! runner '())
  (test-runner-skip-save! runner '())
  (test-runner-fail-save! runner '())
  (test-runner-total-count! runner 0))

(define (test-runner-group-path runner)
  (reverse (test-runner-group-stack runner)))

(define (make-test-runner on-test-begin
			  on-test-end
			  on-group-begin
			  on-group-end
			  on-bad-count
			  on-bad-end-name
			  on-final)
  (define runner
    (%make-test-runner on-test-begin
		       on-test-end
		       on-group-begin
		       on-group-end
		       on-bad-count
		       on-bad-end-name
		       on-final))
  (test-runner-reset runner)
  runner)

;;; Test runners

(define (test-runner-count runner)
  (+ (test-runner-pass-count runner)
     (test-runner-fail-count runner)
     (test-runner-xpass-count runner)
     (test-runner-xfail-count runner)
     (test-runner-skip-count runner)))

(define (test-on-test-begin-simple runner) #f)

(define (test-on-test-end-simple runner)
  (define kind (test-result-kind runner))
  (case kind
    ((pass xpass skip)
     (display "ok "))
    ((fail xfail)
     (display "not ok ")))
  (display (test-runner-count runner))
  (case kind
    ((xfail xpass)
     (display " # TODO"))
    ((skip)
     (display " # SKIP")))
  (let ((test-name (test-runner-test-name runner)))
    (when (< 0 (string-length test-name))
      (case kind ((pass fail) (display " -")))
      (display " ")
      (display test-name)))
  (newline))

(define (test-on-group-begin-simple runner suite-name count)
  (when (null? (test-runner-group-stack runner))
    (display "# Starting test ")
    (display suite-name)
    (newline)))

(define (test-on-group-end-simple runner) #f)

(define (test-on-bad-count-simple runner actual-count expected-count)
  (error "test-on-bad-count-simple: expected count does not match actual count"
	 expected-count actual-count))

(define (test-on-bad-end-name-simple runner begin-name end-name)
  (error "test-on-bad-end-name-simple: test-end does not match test-begin" end-name begin-name))

(define (test-on-final-simple runner)
  (display "1..")
  (display (test-runner-count runner))
  (newline))

(define (test-runner-simple)
  (make-test-runner test-on-test-begin-simple
		    test-on-test-end-simple
		    test-on-group-begin-simple
		    test-on-group-end-simple
		    test-on-bad-count-simple
		    test-on-bad-end-name-simple
		    test-on-final-simple))

(define (test-runner-null)
  (make-test-runner (lambda (runner) #f)
		    (lambda (runner) #f)
		    (lambda (runner suite-name count) #f)
		    (lambda (runner) #f)
		    (lambda (runner actual-count expected-count) #f)
		    (lambda (runner begin-name end-name) #f)
		    (lambda (runner) #f)))

(define current-test-runner (make-parameter #f vector))
(define test-runner-current
  (case-lambda
   (() (vector-ref (current-test-runner) 0))
   ((runner) (vector-set! (current-test-runner) 0 runner))))
(define (test-runner-get)
  (define runner (test-runner-current))
  (or runner
      (error "test-runner-get: test-runner not initialized")))
(define current-test-runner-factory (make-parameter test-runner-simple vector))
(define test-runner-factory
  (case-lambda
   (() (vector-ref (current-test-runner-factory) 0))
   ((runner) (vector-set! (current-test-runner-factory) 0 runner))))
(define (test-runner-create) ((test-runner-factory)))

(define test-begin
  (case-lambda
   ((suite-name)
    (test-begin suite-name #f))
   ((suite-name count)
    (or (test-runner-current) (test-runner-current (test-runner-create)))
    (let ((runner (test-runner-current)))
      ((test-runner-on-group-begin runner) runner suite-name count)
      (test-runner-skip-save! runner
			      (cons (test-runner-skip-list runner)
				    (test-runner-skip-save runner)))
      (test-runner-fail-save! runner
			      (cons (test-runner-fail-list runner)
				    (test-runner-fail-save runner)))
      (test-runner-count-list! runner
			       (cons (cons (test-runner-total-count runner) count)
				     (test-runner-count-list runner)))
      (test-runner-group-stack! runner
				(cons suite-name
				      (test-runner-group-stack runner)))))))

(define test-end
  (case-lambda
   (()
    (test-end #f))
   ((suite-name)
    (let* ((runner (test-runner-get))
	   (groups (test-runner-group-stack runner)))
      (test-result-alist! runner '())
      (when (null? groups)
	(error "test-end: not in a group"))
      (when (and suite-name (not (equal? suite-name (car groups))))
	((test-runner-on-bad-end-name runner) runner suite-name (car groups)))
      (let* ((count-list (test-runner-count-list runner))
	     (expected-count (cdar count-list))
	     (saved-count (caar count-list))
	     (group-count (- (test-runner-total-count runner) saved-count)))
	(when (and expected-count (not (= expected-count group-count)))
	  ((test-runner-on-bad-count runner) runner group-count expected-count))
	((test-runner-on-group-end runner) runner)
	(test-runner-group-stack! runner (cdr (test-runner-group-stack runner)))
	(test-runner-skip-list! runner (car (test-runner-skip-save runner)))
	(test-runner-skip-save! runner (cdr (test-runner-skip-save runner)))
	(test-runner-fail-list! runner (car (test-runner-fail-save runner)))
	(test-runner-fail-save! runner (cdr (test-runner-fail-save runner)))
	(test-runner-count-list! runner (cdr count-list))
	(when (null? (test-runner-group-stack runner))
	  ((test-runner-on-final runner) runner)))))))

(define-syntax test-group
  (syntax-rules ()
    ((test-group suite-name body1 body2 ...)
     (let ((runner (test-runner-current))
	   (name suite-name))
       (test-result-alist! runner `((test-name ,name)))
       (when (test-should-execute runner)
	 (dynamic-wind
	     (lambda () (test-begin name))
	     (lambda () body1 body2 ...)
	     (lambda () (test-end name))))))))

(define-syntax test-group-with-cleanup
  (syntax-rules ()
    ((test-group-with-cleanup suite-name body1 body2 ... cleanup-form)
     (test-group suite-name
		 (dynamic-wind
		     (lambda () #f)
		     (lambda () body1 body2 ...)
		     (lambda () cleanup-form))))))

(define test-result-ref
  (case-lambda
   ((runner property-name)
    (test-result-ref runner property-name #f))
   ((runner property-name default)
    (cond
     ((assq property-name (test-result-alist runner)) => cdr)
     (else default)))))

(define (test-result-set! runner property-name value)
  (define alist (test-result-alist runner))
  (cond
   ((assq property-name alist)
    => (lambda (pair)
	 (set-cdr! pair value)))
   (else (test-result-alist! runner (cons (cons property-name value) alist)))))

(define (test-runner-test-name runner) (test-result-ref runner 'test-name ""))

(define (test-result-clear runner)
  (test-result-alist! runner '()))

(define (test-result-remove runner property-name)
  (test-result-alist! runner
		      (let loop ((alist (test-result-alist runner)))
			(cond
			 ((null? alist)
			  '())
			 ((eq? (caar alist) property-name) (cdr alist))
			 (else (cons (car alist) (loop (cdr alist))))))))

(define test-result-kind
  (case-lambda
   (()
    (test-result-kind (test-runner-current)))
   ((runner)
    (test-result-ref runner 'result-kind))))

(define test-passed?
  (case-lambda
   (()
    (test-passed? (test-runner-get)))
   ((runner)
    (case (test-result-ref runner 'result-kind)
      ((pass xpass) #t)
      (else #f)))))

(define (test-specifier-matches specifier runner) (specifier runner))
(define (test-any-specifier-matches list runner)
  (let ((result #f))
    (for-each
     (lambda (specifier)
       (when (test-specifier-matches specifier runner)
	 (set! result #t)))
     list)
    result))
(define (test-should-execute runner)
  (define run (test-runner-run-list runner))
  (cond
   ((or (not (or (eq? run #t)
		 (test-any-specifier-matches run runner)))
	(test-any-specifier-matches (test-runner-skip-list runner) runner))
    (test-result-set! runner 'result-kind 'skip)
    #f)
   ((test-any-specifier-matches (test-runner-fail-list runner) runner)
    (test-result-set! runner 'result-kind 'xfail)
    'xfail)
   (else #t)))

(define (test-on-test-begin runner)
  (test-should-execute runner)
  ((test-runner-on-test-begin runner) runner)
  (not (eq? 'skip (test-result-ref runner 'result-kind))))

(define (test-on-test-end runner result)
  (test-result-set! runner
		    'result-kind
		    (if (eq? (test-result-ref runner 'result-kind) 'xfail)
			(if result 'xpass 'xfail)
			(if result 'pass 'fail))))

(define-syntax test-evaluate-with-catch
  (syntax-rules ()
    ((test-evaluate-with-catch test-expression)
     (guard (condition (else
			(test-result-set! (test-runner-current) 'actual-error
					  condition)
			#f))
       test-expression))))

(define (test-report-result)
  (define runner (test-runner-get))
  (define result-kind (test-result-kind runner))
  (case result-kind
    ((pass)
     (test-runner-pass-count! runner (+ (test-runner-pass-count runner) 1)))
    ((fail)
     (test-runner-fail-count! runner (+ (test-runner-fail-count runner) 1)))
    ((xpass)
     (test-runner-xpass-count! runner (+ (test-runner-xpass-count runner) 1)))
    ((xfail)
     (test-runner-xfail-count! runner (+ (test-runner-xfail-count runner) 1)))
    (else
     (test-runner-skip-count! runner (+ (test-runner-skip-count runner) 1))))
  (test-runner-total-count! runner (+ (test-runner-total-count runner) 1))
  ((test-runner-on-test-end runner) runner))

(define-syntax test-comp1body
  (syntax-rules ()
    ((test-comp1body runner expression)
     (begin
       (when (test-on-test-begin runner)
	 (let ((result (test-evaluate-with-catch expression)))
	   (test-result-set! runner 'actual-value result)
	   (test-on-test-end runner result)))
       (test-report-result)))))

(define-syntax test-comp2body
  (syntax-rules ()
    ((test-body runner compare expected expression)
     (begin
       (when (test-on-test-begin runner)
	 (let ((e expected))
	   (test-result-set! runner 'expected-value e)
	   (let ((result (test-evaluate-with-catch expression)))
	     (test-result-set! runner 'actual-value result)
	     (test-on-test-end runner (compare e result)))))
       (test-report-result)))))

(define (test-approximate= error)
  (lambda (value expected)
    (<= (- expected error) value (+ expected error))))

(define-syntax test-assert
  (syntax-rules ()
    ((test-assert name test-expression)
     (let ((runner (test-runner-get)))
       (test-result-alist! runner `((test-name . ,name)))
       (test-comp1body runner test-expression)))
    ((test-assert test-expression)
     (let ((runner (test-runner-get)))
       (test-result-alist! runner '())
       (test-comp1body runner test-expression)))))

(define-syntax test-equal
  (syntax-rules ()
    ((test-equal . rest) (test-comp2 equal? . rest))))

(define-syntax test-eqv
  (syntax-rules ()
    ((test-equal . rest) (test-comp2 eqv? . rest))))

(define-syntax test-eq
  (syntax-rules ()
    ((test-equal . rest) (test-comp2 eq? . rest))))

(define-syntax test-approximate
  (syntax-rules ()
    ((test-approximate test-name expected expression error)
     (test-comp2 (test-approximate= error) test-name expression error))
    ((test-approximate expected expression error)
     (test-comp2 (test-approximate= error) expression error))))

(define-syntax test-comp2
  (syntax-rules ()
    ((test-comp2 compare name expected expression)
     (let ((runner (test-runner-get)))
       (test-result-alist! runner `((test-name . ,name)))
       (test-comp2body runner compare expected expression)))
    ((test-comp2 compare expected expression)
     (let ((runner (test-runner-get)))
       (test-result-alist! runner '())
       (test-comp2body runner compare expected expression)))))

(define-syntax test-error
  (syntax-rules ()
    ((test-error name error-type expression)
     (let ((runner (test-runner-get)))
       (test-result-alist! runner `((test-name . ,name)))
       (%test-error runner error-type expression)))
    ((test-error error-type expression)
     (let ((runner (test-runner-get)))
       (test-result-alist! runner '())
       (%test-error runner error-type expression)))
    ((test-error expression)
     (test-error #t expression))))

(define-syntax %test-error
  (syntax-rules ()
    ((%test-error runner error-type expression)
     (test-comp1body runner (guard (condition (error-type #t)) expression #f)))))

(define (test-read-eval-string string)
  (define port (open-input-string string))
  (define form (read port))
  (if (eof-object? (peek-char port))
      (eval form (interaction-environment))
      (error "test-read-eval-string: extra junk after read")))

(define-syntax test-with-runner
  (syntax-rules ()
    ((test-with-runner runner body1 body2 ...)
     (let ((saved-runner (test-runner-current)))
       (dynamic-wind
	   (lambda () (test-runner-current runner))
	   (lambda () body1 body2 ...)
	   (lambda () (test-runner-current saved-runner)))))))

(define (test-apply first . rest)
  (if (test-runner? first)
      (test-with-runner first (apply test-apply rest))
      (let ((runner (test-runner-current)))
	(if runner
	    (let ((run-list (test-runner-run-list runner)))
	      (cond
	       ((null? rest)
                (test-runner-run-list! runner (reverse run-list))
		(first))
	       (else
		(test-runner-run-list! runner
				       (if (eq? run-list #t)
					   (list first)
					   (cons first run-list)))
		(apply test-apply rest)
		(test-runner-run-list! runner run-list))))
	    (let ((runner (test-runner-create)))
	      (test-with-runner runner (apply test-apply first rest))
	      ((test-runner-on-final runner) runner))))))

(define test-match-nth
  (case-lambda
   ((n) (test-match-nth n 1))
   ((n count)
    (let ((i 0))
      (lambda (runner)
	(set! i (+ i 1))
	(and (>= i n) (< i (+ n count))))))))

(define (test-match-all . predicate*)
  (define specifier*
    (map test-as-specifier predicate*))
  (lambda (runner)
    (define result #t)
    (let loop ((specifier* specifier*))
      (cond
       ((null? specifier*)
	result)
       (else
	(unless ((car specifier*) runner)
	  (set! result #f))
	(loop (cdr specifier*)))))))

(define (test-match-any . predicate*)
  (define specifier* (map test-as-specifier predicate*))
  (lambda (runner)
    (define result #f)
    (let loop ((specifier* specifier*))
      (cond
       ((null? specifier*)
	result)
       (else
	(when ((car specifier*) runner)
	  (set! result #t))
	(loop (cdr specifier*)))))))

(define (test-match-name name)
  (lambda (runner)
    (equal? name (test-runner-test-name runner))))

(define (test-skip . predicate*)
  (define runner (test-runner-get))
  (test-runner-skip-list! runner
			  (cons (apply test-match-all predicate*)
				(test-runner-skip-list runner))))

(define (test-expect-fail . predicate*)
  (define runner (test-runner-get))
  (test-runner-fail-list! runner
			  (cons (apply test-match-all predicate*)
				(test-runner-fail-list runner))))

(define (test-as-specifier specifier)
  (cond
   ((procedure? specifier) specifier)
   ((integer? specifier) (test-match-nth 1 specifier))
   ((string? specifier) (test-match-name specifier))
   (else (error "test-as-specifier: invalid test specifier" specifier))))
