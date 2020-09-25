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
;; Bindings ;;
;;;;;;;;;;;;;;

(define-record-type <binding>
  (make-binding type value)
  binding?
  (type %binding-type)
  (value binding-value))

(define (binding-type b) (and b (%binding-type b)))

;;;;;;;;;;;;
;; Stores ;;
;;;;;;;;;;;;

(define (make-store) (list (make-frame)))
(define (make-frame) (make-hash-table label-comparator))
(define (add-frame frame store) (cons frame store))

(define *core-store* (make-store))
(define (new-store) (add-frame (make-frame) *core-store*))

(define (store-ref env lbl)
  (and lbl
       (let f ((env env))
	 (and (not (null? env))
	      (hash-table-ref (car env) lbl (lambda ()
                                              (f (cdr env))))))))

(define (store-set! env lbl b)
  (if b
      (hash-table-set! (car env) lbl b)
      (hash-table-delete! (car env) lbl)))

(define (extend-store env lbl* b*)
  (let ((frame (make-frame)))
    (for-each (lambda (lbl b)
                (hash-table-set! frame lbl b)) lbl* b*)
    (add-frame frame env)))

;;;;;;;;;;;;;;;;;;;;;;;
;; The Current Store ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax with-frame
  (syntax-rules ()
    ((with-frame lbl* b* body1 body2 ...)
     (parameterize ((current-store (extend-store (current-store)
                                                 lbl*
                                                 b*)))
       body1 body2 ...))))

(define-syntax with-bindings
  (syntax-rules ()
    ((with-bindings lbls bs body1 body2 ...)
     (let* ((labels lbls)
            (bindings bs)
            (swap!
             (lambda ()
               (let ((tmps (map lookup labels)))
                 (for-each bind! labels bindings)
                 (set! bindings tmps)))))
       (dynamic-wind
         swap!
         (lambda ()
           body1 body2 ...)
         swap!)))))

(define (lookup lbl)
  (or (store-ref (current-store) lbl)
      (and lbl
           (auxiliary-syntax-label? lbl)
           (make-binding 'macro-parameter
                         (list (auxiliary-syntax (auxiliary-syntax-name lbl))
                               #f)))))

(define (bind! lbl b)
  (when lbl
    (store-set! (current-store) lbl b)))

(define (bind-core! lbl b)
  (store-set! *core-store* lbl b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Current Global Store ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax with-global-store
  (syntax-rules ()
    ((with-global-store body1 body2 ...)
     (parameterize ((current-store (current-global-store)))
       body1 body2 ...))))

(define (bind-global! lbl b)
  (store-set! (current-global-store) lbl b))

(define (current-globals)
  (hash-table->alist (car (current-global-store))))

(define (ref-global loc)
  (hash-table-ref (current-locations) loc))

(define (set-global! loc val)
  (hash-table-set! (current-locations) loc val))

(define (set-keyword! lbl val)
  (list-set! (binding-value (lookup lbl)) 2 val))
