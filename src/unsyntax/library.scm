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

;;;;;;;;;;;;;;;
;; Libraries ;;
;;;;;;;;;;;;;;;

(define-record-type <library>
  (make-library name version imports visreqs invreqs vis-code inv-code visiter
                invoker exports keywords vars)
  library?
  ;; The library name as a list of symbols and non-negative exact integers.
  (name library-name)
  ;; The library version as a list of non-negative exact integers.
  (version library-version)
  ;; The list of directly imported libraries.
  (imports library-imports)
  ;; The list of libraries that have to be invoked [sic!] before
  ;; visiting this library.
  (visreqs library-visit-requirements)
  ;; The list of libraries that have to be invoked before invoking this library.
  (invreqs library-invoke-requirements)
  ;; The library visit code is a list of compiled definitions.
  (vis-code library-visit-code)
  ;; The library invoke code is a list of compiled definitions.
  (inv-code library-invoke-code)
  ;; Thunk that visits the library when called.
  (visiter library-visiter library-set-visiter!)
  ;; Thunk that invokes the library when called.
  (invoker library-invoker library-set-invoker!)
  ;; The library exports as a finite mapping from names to labels with props.
  (exports library-exports)
  ;; The library keywords are an association list from labels to types.
  (keywords library-keywords)
  ;; The library variables are an association list from labels to
  ;; locations.
  (vars library-variables))

;;;;;;;;;;;;;;;;;;
;; Dependencies ;;
;;;;;;;;;;;;;;;;;;

(define (get-dependencies libs)
  (reverse!
   (let ((seen (make-hash-table eq-comparator)))
     (let f ((libs libs) (deps '()))
       (if (null? libs)
           deps
           (let ((deps (f (library-imports (car libs)) deps)))
             (if (hash-table-ref/default seen (car libs) #f)
                 (f (cdr libs) deps)
                 (begin
                   (hash-table-set! seen (car libs) #t)
                   (f (cdr libs) (cons (car libs) deps))))))))))

(define (get-invoke-dependencies libs)
  (reverse!
   (let ((seen (make-hash-table eq-comparator)))
     (let f ((libs libs) (deps '()))
       (if (null? libs)
           deps
           (let ((deps (f (library-invoke-requirements (car libs)) deps)))
             (if (hash-table-ref/default seen (car libs) #f)
                 (f (cdr libs) deps)
                 (begin
                   (hash-table-set! seen (car libs) #t)
                   (f (cdr libs) (cons (car libs) deps))))))))))
