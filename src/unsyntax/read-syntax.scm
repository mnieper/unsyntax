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

;;;;;;;;;;;;;;;;;;;
;; Scheme Reader ;;
;;;;;;;;;;;;;;;;;;;

(define (whitespace? ch)
  (memv ch '(#\space #\newline #\tab #\x0A)))

(define (delimiter? ch)
  (or (whitespace? ch)
      (memv ch '(#\( #\) #\" #\;))
      (eof-object? ch)))

(define (digit-10->number ch)
  (- (char->integer ch) #x30))

(define-record-type <datum-label>
  (%make-datum-label stx refs)
  datum-label?
  (stx datum-label-stx datum-label-set-stx!)
  (refs datum-label-refs datum-label-set-refs!))

(define (hex-digit ch)
  (cond
   ((eof-object? ch)
    #f)
   ((char<=? #\0 ch #\9)
    (- (char->integer ch) #x30))
   ((char<=? #\A ch #\F)
    (- (char->integer ch) #x37))
   ((char<=? #\a ch #\f)
    (- (char->integer ch) #x57))
   (else
    #f)))

(define (hex-scalar-value token)
  (and-let*
      (((> (string-length token) 0))
       (ch (string-ref token 0))
       ((or (char=? #\x ch) (char=? #\X ch))))
    (let f ((value #f) (digits (cdr (string->list token))))
      (cond
       ((null? digits)
	value)
       ((hex-digit (car digits))
	=> (lambda (digit)
	     (f (+ (* 16 (or value 0)) digit) (cdr digits))))
       (else
	#f)))))

(define (make-datum-label)
  (%make-datum-label #f '()))

(define (join-references! lbl1 lbl2)
  (datum-label-set-refs! lbl1 (append (datum-label-refs lbl1)
				      (datum-label-refs lbl2))))

(define (label-add-reference! lbl ref)
  (datum-label-set-refs! lbl (cons ref
				   (datum-label-refs lbl))))

(define (clear-references! lbl)
  (datum-label-set-refs! lbl '()))

(define (for-each-reference proc lbl)
  (for-each proc (datum-label-refs lbl)))

(define (read-syntax sp)
  (define datum-labels (make-hash-table eqv-comparator))
  (define reference-table (make-hash-table eq-comparator))
  (define (add-reference! stx lbl)
    (hash-table-set! reference-table stx lbl)
    (label-add-reference! lbl stx))
  (define (references stx)
    (hash-table-ref/default reference-table stx #f))
  (define filename (source-port-filename sp))
  (define (peek-char) (source-port-peek sp))
  (define (read-char) (source-port-read sp))
  (define (position) (source-port-position sp))
  (define (fold-case s)
    (if (source-port-ci? sp)
	(string-downcase s)
	s))
  (define (read-intertoken-space)
    (define ch (peek-char))
    (case ch
      ((#\;)
       (read-comment)
       (read-intertoken-space))
      (else
       (when (whitespace? ch)
	 (read-char)
	 (read-intertoken-space)))))
  (define (read-comment)
    (define ch (read-char))
    (unless (or (eof-object? ch)
		(char=? #\newline ch))
      (read-comment)))
  (define (read-token . char*)
    (let f ((char* char*))
      (if (delimiter? (peek-char))
	  (list->string (reverse! char*))
	  (f (cons (read-char) char*)))))
  (define (read-uinteger-10 first-digit)
    (let f ((val (digit-10->number first-digit)))
      (case (peek-char)
	((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	 (f (+ (* 10 val) (digit-10->number (read-char)))))
	(else
	 val))))
  (let read ((handle-dot
	      (lambda (reader-error)
		(reader-error "unexpected dot")))
	     (handle-closing-parenthesis
	      (lambda (reader-error)
		(reader-error "unexpected closing parenthesis"))))
    (define (read-datum) (read handle-dot handle-closing-parenthesis))
    (read-intertoken-space)
    (let ((start (position)))
      (define (datum-label-intern! n)
	(define lbl (make-datum-label))
	(hash-table-update! datum-labels n values
			    (lambda () lbl)
			    (lambda (prev-label)
			      (reader-error "duplicate datum label ‘~a’" n)))
	lbl)
      (define (datum-label n)
	(hash-table-ref datum-labels n
			(lambda ()
			  (reader-error "unknown datum label ‘~a’" n))))
      (define (make-source-location)
	(define end (position))
	(source-location filename start end))
      (define (make-syntax e)
	(make-syntax-object e '() '() (make-source-location)))
      (define (reader-error msg . arg*)
	(apply raise-reader-error (make-source-location) msg arg*))
      (define (code-point->character val)
	(unless (or (<= 0 val #xD7FF)
		    (<= #xE000 val #x10FFFF))
	  (reader-error "invalid unicode code point ‘#x~s’"
                        (number->string val 16)))
	(integer->char val))
      (define (read-abbreviation name)
	(define id (make-syntax name))
	(make-syntax (list id (read-datum))))
      (define (read-character)
	(define ch (read-char))
	(when (eof-object? ch)
	  (reader-error "incomplete character"))
	(let ((token (string-append (string ch) (read-token))))
	  (cond
	   ((= 1 (string-length token))
	    (make-syntax ch))
	   ((hex-scalar-value token)
	    => (lambda (val)
		 (make-syntax (code-point->character val))))
	   (else
	    (case (string->symbol (fold-case token))
	      ((alarm) (make-syntax #\alarm))
	      ((backspace) (make-syntax #\backspace))
	      ((delete) (make-syntax #\delete))
	      ((escape) (make-syntax #\escape))
	      ((newline) (make-syntax #\newline))
	      ((null) (make-syntax #\null))
	      ((return) (make-syntax #\return))
	      ((space) (make-syntax #\space))
	      ((tab) (make-syntax #\tab))
	      (else
	       (reader-error "invalid character name ‘~a’" token)))))))
      ;; TODO: Handle start and end of bytevector (and other structure) correctly.
      (define (read-bytevector)
	(unless (and (char=? #\8 (read-char))
		     (char=? #\( (read-char)))
	  (reader-error "invalid bytevector"))
	(call/cc
	 (lambda (k)
	   (let f ((byte* '()))
	     (define (handle-dot reader-error)
	       (reader-error "unexpected dot"))
	     (define (handle-closing-parenthesis reader-error)
	       (k (make-syntax (apply bytevector (reverse! byte*)))))
	     (define datum (read handle-dot handle-closing-parenthesis))
	     (when (eof-object? datum)
	       (reader-error "incomplete bytevector"))
	     (let ((val (syntax-object-expr datum)))
	       (unless (and (exact-integer? val)
			    (<= 0 val 255))
		 (reader-error "invalid byte"))
	       (f (cons val byte*)))))))
      (define (read-string q)
        (define symbol? (char=? q #\|))
        (let f ((ch* '()))
          (case (read-char)
            ((#\\)
             (case (read-char)
               ((#\")
                (f (cons #\" ch*)))
               ((#\\)
                (f (cons #\\ ch*)))
               ((#\n)
                (f (cons #\newline ch*)))
               ;; TODO: More escapes.
               (else
                => (lambda (ch)
                     (reader-error "invalid escape `\\~a' in string" ch)))))
            (else
             => (lambda (ch)
                  (cond
                   ((eof-object? ch)
                    (reader-error (if symbol? "incomplete symbol"
                                      "incomplete string")))
                   ((char=? ch q)
                    (make-syntax ((if symbol? string->symbol
                                      values)
                                  (list->string (reverse! ch*)))))
                   (else
                    (f (cons ch ch*)))))))))
      (let ((ch (read-char)))
	(case ch
	  ((#\()
	   (call/cc
	    (lambda (k)
	      (let f ((datum* '()))
		(define (handle-dot reader-error)
		  (let f ((tail '()))
		    (define (handle-dot reader-error)
		      (reader-error "unexpected dot"))
		    (define (handle-closing-parenthesis reader-error)
		      (if (null? tail)
			  (reader-error "element missing after dot")
			  (k (make-syntax (append-reverse! datum*
							   (car tail))))))
		    (define datum (read handle-dot handle-closing-parenthesis))
		    (cond
		     ((eof-object? datum)
		      (reader-error "incomplete list tail"))
		     ((pair? tail)
		      (reader-error "multiple tokens in dotted tail"))
		     (else
		      (f (list datum))))))
		(define (handle-closing-parenthesis reader-error)
		  (k (make-syntax (reverse! datum*))))
		(define datum (read handle-dot handle-closing-parenthesis))
		(if (eof-object? datum)
		    (reader-error "incomplete list")
		    (f (cons datum datum*)))))))
	  ((#\)) (handle-closing-parenthesis reader-error))
	  ;; Abbreviations
	  ((#\') (read-abbreviation 'quote))
	  ((#\`) (read-abbreviation 'quasiquote))
	  ((#\,)
	   (case (peek-char)
	     ((#\@)
	      (read-char)
	      (read-abbreviation 'unquote-splicing))
	     (else
	      (read-abbreviation 'unquote))))
	  ;; Strings
	  ((#\")
           (read-string #\"))
          ;; Symbols
          ((#\|)
           (read-string #\|))
	  ;; Sharp syntax
	  ((#\#)
	   (case (read-char)
	     ;; Datum comment
	     ((#\;)
	      (read-datum)
	      (read-datum))
	     ;; Syntax abbreviations
	     ((#\')
	      (read-abbreviation 'syntax))
	     ((#\,)
	      (case (peek-char)
		((#\@)
		 (read-char)
		 (read-abbreviation 'unsyntax-splicing))
		(else
		 (read-abbreviation 'unsyntax))))
	     ((#\`)
	      (read-abbreviation 'quasisyntax))
	     ;; Characters
	     ((#\\) (read-character))
	     ;; Vectors
	     ((#\()
	      (call/cc
	       (lambda (k)
		 (let f ((datum* '()))
		   (define (handle-dot reader-error)
		     (reader-error "unexpected dot"))
		   (define (handle-closing-parenthesis reader-error)
		     (k (make-syntax (list->vector (reverse! datum*)))))
		   (define datum (read handle-dot handle-closing-parenthesis))
		   (if (eof-object? datum)
		       (reader-error "incomplete vector")
		       (f (cons datum datum*)))))))
	     ((#\u) (read-bytevector))
	     ((#\t #\T) (make-syntax #t))
	     ((#\f #\F) (make-syntax #f))
	     ;; Numbers
	     ((#\b #\o #\d #\x)
	      => (lambda (prefix)
		   (define radix
		     (case prefix
		       ((#\b) 2) ((#\o) 8) ((#\d) 10) ((#\x) 16)))
		   (define token (read-token))
		   (define num (string->number token radix))
		   (unless num
		     (reader-error "invalid number literal"))
		   (make-syntax num)))
	     ;; Datum labels
	     ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	      => (lambda (ch)
		   (define n (read-uinteger-10 ch))
		   (case (read-char)
		     ((#\=)
		      (let* ((lbl (datum-label-intern! n))
			     (referenced-stx (read-datum)))
			(when (eof-object? referenced-stx)
			  (reader-error "datum missing after datum label"))
			(datum-label-set-stx! lbl referenced-stx)
			(cond
			 ;; The referenced syntax is a reference itself.
			 ((references referenced-stx)
			  => (lambda (referenced-lbl)
			       (when (eq? lbl referenced-lbl)
				 (reader-error "datum label ‘~a’ references itself" n))
			       (join-references! referenced-lbl lbl)))
			 ;; The references so far can be patched.
			 (else
			  (let ((obj (syntax-object-expr referenced-stx)))
			    (for-each-reference (lambda (ref)
						  (syntax-object-set-expr! ref obj))
						lbl)
			    (clear-references! lbl))))
			referenced-stx))
		     ((#\#)
		      (let ((lbl (datum-label n)))
			(cond
			 ((datum-label-stx lbl)
			  => (lambda (referenced-stx)
			       (make-syntax referenced-stx)))
			 (else
			  (let ((referencing-stx (make-syntax #f)))
			    (add-reference! referencing-stx lbl)
			    referencing-stx)))))
		     (else
		      => (lambda (ch)
			   (if (eof-object? ch)
			       (reader-error "incomplete datum label syntax")
			       (reader-error "invalid datum label syntax")))))))
	     (else
	      => (lambda (ch)
		   (if (eof-object? ch)
		       (reader-error "incomplete sharp syntax")
		       (reader-error "invalid sharp syntax ‘~a’" ch))))))
	  (else
	   (cond
	    ((eof-object? ch) ch)
	    (else
	     (let ((token (read-token ch)))
	       (cond
		((string=? "." token)
		 (handle-dot reader-error))
		((string->number token)
		 => (lambda (num)
		      (make-syntax num)))
		(else
		 ;; TODO: Reject invalid identifier syntax.
		 (make-syntax (string->symbol (fold-case token))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience Procedures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-syntax* sp)
  (generator->list (lambda () (read-syntax sp))))

(define (read-source source)
  (guard (exc
	  ((file-error? exc)
	   (raise-error #f "error reading file: ~a" source)))
    (call-with-input-file source
      (lambda (in)
	(define sp (make-source-port in #f source))
	(read-syntax* sp)))))

(define (read-program source)
  (datum->syntax #f (read-source source) (source-location source #f #f)))

(define (read-file stx ci?)
  (let ((filename (syntax-object-expr stx)))
    (unless (string? filename)
      (raise-syntax-error stx "invalid filename"))
    (let* ((loc (syntax-object-srcloc stx))
           (source (and loc (source-location-source loc)))
           (vicinity (if source
                         (pathname->vicinity source)
                         (user-vicinity)))
           (source (in-vicinity vicinity filename)))
      (guard (exc
	      ((file-error? exc)
	       (raise-syntax-error stx "error reading file")))
	(call-with-input-file source
	  (lambda (in)
	    (define sp (make-source-port in ci? source))
	    (read-syntax* sp)))))))

(define (read-file* ls ci?)
  (let loop ((ls ls))
    (if (null? ls)
        '()
        (let ((stx (read-file (car ls) ci?)))
          (append stx (loop (cdr ls)))))))

(define read
  (case-lambda
    (()
     (read (current-input-port)))
    ((in)
     (let ((stx (read-syntax (make-source-port in #f #f))))
       (if (eof-object? stx)
           stx
           (syntax->datum stx))))))
