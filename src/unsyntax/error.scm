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

;;;;;;;;;;;;
;; Prefix ;;
;;;;;;;;;;;;

(define interactive? (make-parameter #f))
(define (prefix) (if (interactive?) "error" (program-invocation-short-name)))

;;;;;;;;;;;;;;;;;
;; User Errors ;;
;;;;;;;;;;;;;;;;;

(define-record-type <error>
  (make-error who message irritants)
  error?
  (who error-who)
  (message error-message)
  (irritants error-irritants))

(define (format-error e)
  (format "~a: ~a~a"
          (prefix)
          (format-who e)
          (format-message e)))

(define (format-message e)
  (apply format (error-message e) (error-irritants e)))

(define (format-who e)
  (cond ((error-who e)
         => (lambda (who)
              (format "~a: " who)))
        (else "")))

(define (raise-error who message . irritants)
  (raise (make-error who message irritants)))

;;;;;;;;;;;;;;;;;;;
;; Reader Errors ;;
;;;;;;;;;;;;;;;;;;;

(define-record-type <reader-error>
  (make-reader-error loc message irritants)
  reader-error?
  (loc reader-error-location)
  (message reader-error-message)
  (irritants reader-error-irritants))

(define (format-reader-error e)
  (format "~a: ~a"
          (format-reader-location e)
          (format-reader-message e)))

(define (format-reader-message e)
  (apply format (reader-error-message e) (reader-error-irritants e)))

(define (format-reader-location e)
  (format-source-location (reader-error-location e)))

(define (raise-reader-error loc message . irritants)
  (raise (make-reader-error loc message irritants)))

;;;;;;;;;;;;;;;;;;;
;; Syntax Errors ;;
;;;;;;;;;;;;;;;;;;;

(define-record-type <syntax-error>
  (make-syntax-error stx message irritants)
  syntax-error?
  (stx syntax-error-object)
  (message syntax-error-message)
  (irritants syntax-error-irritants))

(define (format-syntax-error e)
  (format "~a: ~a"
          (format-syntax-object e)
          (format-syntax-message e)))

(define (format-syntax-message e)
  (apply format (syntax-error-message e) (syntax-error-irritants e)))

(define (format-syntax-object e)
  (format-source-location (and-let* ((stx (syntax-error-object e)))
                            (syntax-object-srcloc stx))))

(define (format-source-location srcloc)
  (if (and srcloc
           (source-location-source srcloc))
      (let*
          ((start (source-location-start srcloc))
           (end (source-location-end srcloc))
           (from (if start
                     (format ": ~a.~a"
                             (position-line start)
                             (+ 1 (position-column start)))
                     ""))
           (to (if end
                   (if (= (position-line start) (position-line end))
                       (format "-~a" (position-column end))
                       (format "-~a.~a"
                               (position-line end)
                               (+ 1 (position-column end))))
                   "")))
        (string-append (source-location-source srcloc)
                       from
                       to))
      (prefix)))

(define (raise-syntax-error stx message . irritants)
  (raise (make-syntax-error (and (syntax-object? stx) stx) message irritants)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Error Handler ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-error-handler thunk)
  (with-exception-handler
      (lambda (e)
        (cond ((error? e)
               (print-error/die (format-error e)))
              ((syntax-error? e)
               (print-error/die (format-syntax-error e)))
              ((reader-error? e)
               (print-error/die (format-reader-error e)))
              (else
               (flush-output-port (current-output-port))
               (write-string (format "~a: internal error~%"
                                     (prefix)))
               (raise e))))
    thunk))

(define (with-interactive-error-handler thunk)
  (parameterize ((interactive? #t))
    (guard
        (e
         ((error? e)
          (flush-input-port)
          (print-error (format-error e)))
         ((syntax-error? e)
          (flush-input-port)
          (print-error (format-syntax-error e)))
         ((reader-error? e)
          (flush-input-port)
          (print-error (format-reader-error e)))
         ((error-object? e)
          (flush-input-port)
          (print-error (format-error-object e)))
         (else
          (flush-input-port)
          (print-error (format "error: ~s" e))))
      (thunk))))

(define (flush-input-port)
  (when (char-ready?) (read-line)))

(define (format-error-object e)
  (format "error: ~a: ~s" (error-object-message e) (error-object-irritants e)))

(define (print-error s)
  (flush-output-port (current-output-port))
  (write-string s (current-error-port))
  (newline (current-error-port)))

(define (print-error/die s)
  (print-error s)
  (exit 1))
