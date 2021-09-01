;; Copyright © Marc Nieper-Wißkirchen (2020).

;; This file is part of Unsyntax.

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

;;;;;;;;;;;;
;; Labels ;;
;;;;;;;;;;;;

(define (make-label) (gensym "l"))
(define label=? eq?)
(define (label? obj) (symbol? obj))
(define label-comparator eq-comparator)

(define (make-label/props label alist) (cons label alist))
(define (label/props-label l/p) (car l/p))
(define (label/props-props l/p) (cdr l/p))
(define (label/props-add l/p key-lbl prop-lbl)
  (make-label/props (label/props-label l/p)
                    (alist-cons key-lbl prop-lbl (label/props-props l/p))))

(define (label/props-merge new-l/p prev-l/p)
  (let ((lbl (label/props-label new-l/p)))
    (if (label=? lbl (label/props-label prev-l/p))
        (make-label/props lbl
                          (append (label/props-props new-l/p)
                                  (label/props-props prev-l/p)))
        new-l/p)))

(define (label/props-filter pred l/p)
  (cons (label/props-label l/p)
        (filter (lambda (entry)
                  (and (pred (car entry)) (pred (cdr entry))))
                (label/props-props l/p))))

(define (ref-prop l/p id-lbl key-lbl k)
  (if l/p
      (and (label=? id-lbl (label/props-label l/p))
           (cond
            ((assoc key-lbl (label/props-props l/p) label=?) => cdr)
            (else
             (k))))
      (k)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Substitution Ribs ;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; A rib is a list of the form (MUTABLE? TABLE BARRIER ... TABLE
;;; BARRIER TABLE) where MUTABLE? is #t or #f depending on whether the
;;; rib is mutable, TABLEs are identifier tables mapping identifiers
;;; to labels with props, and BARRIERs are lists of lists of marks.

(define rib-mutable? car)

(define rib-chunks cdr)
(define rib-set-chunks! set-cdr!)

(define (next-chunk chunks)
  (let ((chunk (car chunks)))
    (define table
      (if (identifier-table? chunk)
          chunk
          (let ((chunk (datum->identifier-table chunk)))
            (set-car! chunks chunk)
            chunk)))
    (let ((chunks (cdr chunks)))
      (if (null? chunks)
          (values table #f #f)
          (values table (car chunks) (cdr chunks))))))

(define make-rib
  (case-lambda
    (()
     (list #f (make-identifier-table)))
    ((id* lbl*)
     (let ((table (make-identifier-table)))
       (define (add! id lbl)
	 (identifier-table-set! table id (make-label/props lbl '())))
       (for-each add! id* lbl*)
       (list #f table)))))

(define (make-mutable-rib)
  (list #t (make-identifier-table)))

(define (barrier-blocks? barrier id)
  (and (member (syntax-object-marks id) barrier marks=?) #t))

(define (rib-add-barrier! rib mark**)
  (rib-set-chunks! rib (cons* (make-identifier-table)
                              (delete-duplicates mark** marks=?)
                              (rib-chunks rib))))

(define (rib-ref/props rib id)
  (let f ((chunks (rib-chunks rib)))
    (receive (table barrier chunks) (next-chunk chunks)
      (or (identifier-table-ref table id)
          (and barrier
	       (not (barrier-blocks? barrier id))
               (f chunks))))))

(define (rib-ref rib id)
  (and-let* ((l/p (rib-ref/props rib id)))
    (label/props-label l/p)))

(define (rib-ref/props/failure rib id failure)
  (let f ((chunks (rib-chunks rib)))
    (receive (table barrier chunks) (next-chunk chunks)
      (or (identifier-table-ref table id)
	  (if barrier
	      (and (not (barrier-blocks? barrier id))
		   (f chunks))
	      (failure))))))

(define (rib-ref/props/failure+success rib id failure success)
  (let f ((chunks (rib-chunks rib)))
    (receive (table barrier chunks) (next-chunk chunks)
      (cond
       ((identifier-table-ref table id) => success)
       (else
	(if barrier
	    (and (not (barrier-blocks? barrier id))
		 (f chunks))
	    (failure)))))))

(define (rib-set!/props rib id l/p)
  (define chunks (rib-chunks rib))
  (receive (table barrier chunks) (next-chunk chunks)
    (define (failure)
      (identifier-table-set! table id l/p))
    (define (success cell)
      (define prev-l/p (identifier-table-cell-ref cell))
      (unless (or (rib-mutable? rib)
		  (label=? (label/props-label l/p)
			   (label/props-label prev-l/p)))
	(raise-syntax-error
	 id "duplicate definition of identifier ‘~a’"
	 (identifier-name id)))
      (identifier-table-cell-set! cell (label/props-merge l/p prev-l/p)))
    (cond
     ((identifier-table-cell table id) => success)
     (else
      (let f ((barrier barrier) (chunks chunks))
	(if (or (not barrier) (barrier-blocks? barrier id))
	    (failure)
	    (receive (table barrier chunks) (next-chunk chunks)
	      (cond
	       ((identifier-table-cell table id) => success)
	       (else (f barrier chunks))))))))))

(define (rib-set! rib id lbl)
  (rib-set!/props rib id (make-label/props lbl '())))

(define (rib->datum rib label-present?)
  (define (filter l/p)
    (and (label-present? (label/props-label l/p))
	 (label/props-filter label-present? l/p)))
  (define datum
    (let f ((chunks (rib-chunks rib)))
      (receive (table barrier chunks) (next-chunk chunks)
	(if barrier
	    (cons* (identifier-table->datum table filter)
		   barrier
		   (f chunks))
	    (list (identifier-table->datum table filter))))))
  (and (not (list= eq? datum '())) datum))

;;;;;;;;;;;;;;;;;;;;;;
;; Resolving labels ;;
;;;;;;;;;;;;;;;;;;;;;;

(define current-global-resolver
  (make-parameter (lambda (id) #f)))

(define (resolve id)
  (and-let* ((l/p (resolve/props id)))
    (label/props-label l/p)))

(define (resolve/props id)
  (or (let ((name (identifier-name id)))
        (let f ((m* (syntax-object-marks id))
                (s* (syntax-object-substs id)))
          (and (not (null? s*))
               (if (shift? (car s*))
                   (f (cdr m*) (cdr s*))
                   (rib-ref/props/failure (car s*) (make-identifier name m*)
					  (lambda ()
					    (f m* (cdr s*))))))))
      ((current-global-resolver) id)))

(define (resolve-prop id id-lbl key-lbl)
  (or (let ((name (identifier-name id)))
        (let f ((m* (syntax-object-marks id))
                (s* (syntax-object-substs id)))
          (and (not (null? s*))
               (if (shift? (car s*))
                   (f (cdr m*) (cdr s*))
		   (rib-ref/props/failure+success
		    (car s*) (make-identifier name m*)
		    (lambda ()
		      (f m* (cdr s*)))
		    (lambda (l/p)
		      (ref-prop l/p
				id-lbl
				key-lbl
				(lambda ()
				  (f m* (cdr s*))))))))))
      (ref-prop ((current-global-resolver) id) id-lbl key-lbl
                (lambda () #f))))

(define (free-identifier=? id1 id2)
  (let ((l1 (resolve id1))
        (l2 (resolve id2)))
    (if (or l1 l2)
        (label=? l1 l2)
        (symbol=? (identifier-name id1) (identifier-name id2)))))
