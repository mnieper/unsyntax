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

;;; TODO: Clean up the code.

;;;;;;;;;;;
;; Marks ;;
;;;;;;;;;;;

(define (make-mark) (gensym "m"))

(define (anti-mark) #f)
(define (anti-mark? mark) (eq? #f mark))

(define (marks=? m1* m2*)
  (or (and (null? m1*) (null? m2*))
      (and (pair? m1*) (pair? m2*)
           (eq? (car m1*) (car m2*))
           (marks=? (cdr m1*) (cdr m2*)))))

;;;;;;;;;;;;;;;;
;; Unwrapping ;;
;;;;;;;;;;;;;;;;

(define (unwrap-syntax stx)
  (if (syntax-object? stx)
      (syntax-object-expr stx)
      stx))

;;;;;;;;;;;;;;
;; Wrapping ;;
;;;;;;;;;;;;;;

(define (shift) 'shift)
(define (shift? s) (eq? 'shift s))

(define (add-mark mark stx)
  (syntax-object stx (list mark) (list (shift)) #f))

(define (add-substs env stx)
  (syntax-object stx '() (list env) #f))

(define (add-substs* env stx*)
  (syntax-map (lambda (stx)
                (add-substs env stx))
              stx*))

(define (append-marks x y)
  (if (null? y)
      x
      (append x y)))

(define (append/cancel x y)
  (pair-fold-right (lambda (x y)
		     (if (null? (cdr x))
			 y
			 (cons (car x) y)))
		   (cdr y) x))

(define (append-substs s* t*)
  (if (null? t*)
      s*
      (fold-right (lambda (s t*)
		    (cons-substs s t*))
		  t* s*)))

(define (append-substs/cancel s* t*)
  (pair-fold-right (lambda (s* t*)
		     (if (null? (cdr s*))
			 t*
			 (cons-substs (car s*) t*)))
		   (cdr t*) s*))

(define (cons-substs s s*)
  (if (and (not (shift? s))
	   (not (null? s*))
	   (eq? s (car s*)))
      s*
      (cons s s*)))

(define (join-wraps m1* s1* stx)
  (let ((m2* (syntax-object-marks stx))
	(s2* (syntax-object-substs stx)))
    (if (and (not (null? m1*))
	     (not (null? m2*))
	     (anti-mark? (car m2*)))
	(values (append/cancel m1* m2*)
		(append-substs/cancel s1* s2*))
	(values (append-marks m1* m2*)
		(append-substs s1* s2*)))))

(define (syntax-object e m* s* loc)
  (if (syntax-object? e)
      (receive (m* s*) (join-wraps m* s* e)
	(make-syntax-object (syntax-object-expr e) m* s*
			    (or (syntax-object-srcloc e) loc)))
      (make-syntax-object e m* s* loc)))

;;;;;;;;;;;;;;;;;;
;; Syntax Lists ;;
;;;;;;;;;;;;;;;;;;

(define (syntax-null? stx)
  (null? (unwrap-syntax stx)))

(define (syntax-pair? stx)
  (pair? (unwrap-syntax stx)))

(define (syntax-car stx)
  (if (syntax-object? stx)
      (let ((e (syntax-object-expr stx))
	    (m* (syntax-object-marks stx))
	    (s* (syntax-object-substs stx))
	    (loc (syntax-object-srcloc stx)))
	(syntax-object (car e) m* s* loc))
      (car stx)))

(define (syntax-cdr stx)
  (if (syntax-object? stx)
      (let ((e (syntax-object-expr stx))
	    (m* (syntax-object-marks stx))
	    (s* (syntax-object-substs stx))
	    (loc (syntax-object-srcloc stx)))
	(syntax-object (cdr e) m* s* loc))
      (cdr stx)))

(define (syntax-length+ stx)
  (let ((e (unwrap-syntax stx)))
    (let f ((e e) (lag e) (len 0))
      (if (pair? e)
	  (let ((e (unwrap-syntax (cdr e)))
		(len (+ 1 len)))
	    (if (pair? e)
		(let ((e (unwrap-syntax (cdr e)))
		      (lag (unwrap-syntax (cdr lag)))
		      (len (+ 1 len)))
		  (and (not (eq? e lag))
		       (f e lag len)))
		len))
	  len))))

(define (syntax-split-at stx i)
  (let f ((stx stx) (i i))
    (if (zero? i)
	(values '() stx)
	(receive (prefix suffix) (f (syntax-cdr stx) (- i 1))
	  (values (cons (syntax-car stx) prefix) suffix)))))

(define (syntax-circular-list? stx)
  (let ((e (unwrap-syntax stx)))
    (let f ((e e) (lag e))
      (and (pair? e)
	   (let ((e (unwrap-syntax (cdr e))))
	     (and (pair? e)
		  (let ((e (unwrap-syntax (cdr e)))
			(lag (unwrap-syntax (cdr lag))))
		    (or (eq? e lag)
			(f e lag)))))))))

(define (syntax->list stx)

  #;
  (let f ((stx stx) (lag stx) (res '()))
    (cond ((syntax-null? stx) (reverse! res))
	  ((syntax-pair? stx)
	   (let ((res (cons (syntax-car stx) res))
		 (stx (syntax-cdr stx)))
	     (cond ((syntax-null? stx) (reverse! res))
		   ((syntax-pair? stx)
		    (let ((res (cons (syntax-car stx) res))
			  (stx (syntax-cdr stx))
			  (lag (syntax-cdr lag)))
		      (and (not (eq? (unwrap-syntax stx)
				     (unwrap-syntax lag)))
			   (f stx lag res))))
		   (else #f))))
	  (else #f)))



  (and (not (syntax-circular-list? stx))
       (let f ((stx stx) (res '()))
	 (cond ((syntax-null? stx)
		(reverse! res))
	       ((syntax-pair? stx)
		(f (syntax-cdr stx) (cons (syntax-car stx) res)))
	       (else #f)))))

(define (syntax-map proc stx)
  (let f ((stx stx))
    (if (syntax-null? stx)
	'()
	(cons (proc (syntax-car stx))
	      (f (syntax-cdr stx))))))

;;;;;;;;;;;;;;;;;;;;
;; Syntax Vectors ;;
;;;;;;;;;;;;;;;;;;;;

(define (syntax-vector? stx)
  (vector? (unwrap-syntax stx)))

(define (syntax-vector->list stx)
  (if (syntax-object? stx)
      (let ((e (syntax-object-expr stx))
	    (m* (syntax-object-marks stx))
	    (s* (syntax-object-substs stx))
	    (loc (syntax-object-substs stx)))
	(map (lambda (stx)
	       (syntax-object stx m* s* loc))
	     (vector->list e)))
      (vector->list stx)))

;;;;;;;;;;;;;;;;;;;
;; syntax->datum ;;
;;;;;;;;;;;;;;;;;;;

(define (simple-datum? e)
  (or (boolean? e)
      (bytevector? e)
      (char? e)
      (null? e)
      (number? e)
      (string? e)
      (symbol? e)))

(define (syntax->datum stx)
  (define table (make-hash-table eq-comparator))
  (let loop ((stx stx))
    (define datum (if (syntax-object? stx) (syntax-object-expr stx) stx))
    (cond
     ((pair? datum)
      (or (hash-table-ref/default table datum #f)
          (let ((pair (cons #f #f)))
            (hash-table-set! table datum pair)
            (set-car! pair (loop (car datum)))
            (set-cdr! pair (loop (cdr datum)))
            pair)))
     ((vector? datum)
      (or (hash-table-ref/default table datum #f)
          (let ((vector (make-vector (vector-length datum))))
            (hash-table-set! table datum vector)
            (do ((i 0 (+ i 1)))
                ((= i (vector-length datum)))
              (vector-set! vector i (loop (vector-ref datum i))))
            vector)))
     ((simple-datum? datum)
      datum))))

;;;;;;;;;;;;;;;;;;;
;; datum->syntax ;;
;;;;;;;;;;;;;;;;;;;

(define datum->syntax
  (case-lambda
    ((id datum)
     (datum->syntax id datum #f))
    ((id datum loc)
     (if id
	 (syntax-object datum
			(syntax-object-marks id)
			(syntax-object-substs id)
			loc)
	 (syntax-object datum '() '() loc)))))
