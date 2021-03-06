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

;;;;;;;;;;;;;;
;; Contexts ;;
;;;;;;;;;;;;;;

(define-record-type <context>
  (make-context expanded-libraries libraries globals locations)
  context?
  (expanded-libraries context-expanded-libraries context-set-expanded-libraries!)
  (libraries context-library-table)
  (globals context-global-store)
  (locations context-locations))

(define (new-context)
  (make-context '()
		(make-library-table)
		(new-store)
                (make-hash-table variable-comparator)))

(define current-context (make-parameter (new-context)))

(define (record-expanded-library! lib)
  (when lib
    (context-set-expanded-libraries!
     (current-context)
     (cons lib (context-expanded-libraries (current-context))))))

(define (expanded-libraries)
  (reverse (context-expanded-libraries (current-context))))

(define (current-library-table)
  (context-library-table (current-context)))

(define (current-global-store)
  (context-global-store (current-context)))

(define (current-locations) (context-locations (current-context)))

(define current-meta-level (make-parameter 0))

(define current-meta-store (make-parameter (current-global-store)))
(define current-store (make-parameter (current-meta-store)))

(define-syntax with-new-context
  (syntax-rules ()
    ((with-new-context body1 body2 ...)
     (parameterize ((current-context (new-context))
                    (current-meta-level 0))
       (parameterize ((current-meta-store (current-global-store)))
         (parameterize ((current-store (current-meta-store)))
           body1 body2 ...))))))

(define-syntax in-meta
  (syntax-rules ()
    ((in-meta-level body1 body2 ...)
     (parameterize ((current-meta-level (+ 1 (current-meta-level))))
       body1 body2 ...))))
