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

;;; During expansion, a current meta level is maintained.  During
;;; expansion of the right hand sides of syntax or meta definitions,
;;; the meta level is increased by one.

;;; A binding also has a meta level. If the meta level is
;;; non-negative, the binding is only available at a phase with the
;;; same meta level.  If the meta level META-LEVEL is negative, the
;;; binding is available at phases such that CURRENT-META-LEVEL >= -
;;; META-LEVEL - 1.

(define-record-type <binding>
  (%make-binding type value meta-level)
  binding?
  (type %binding-type)
  (value binding-value)
  (meta-level binding-meta-level))

(define (make-runtime-binding type value)
  (%make-binding type value (current-meta-level)))

(define (make-binding type value)
  (%make-binding type value (- -1 (current-meta-level))))

(define (make-meta-binding type value)
  (%make-binding type value (- -2 (current-meta-level))))

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
                                                 b*))
                    (current-meta-store (extend-store (current-meta-store)
                                                      '()
                                                      '())))
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

(define (current-store-ref lbl)
  (store-ref (current-store) lbl))

(define (lookup lbl)
  (and-let* ((binding
              (or (current-store-ref lbl)
                  (and lbl
                       (auxiliary-syntax-label? lbl)
                       (make-binding
                        'macro-parameter
                        (list (auxiliary-syntax (auxiliary-syntax-name lbl))
                              #f))))))
    (if (binding? binding)
        (let ((ml (binding-meta-level binding))
              (cml (current-meta-level)))
          (if (or (= ml cml)
                  (and (< ml 0) (<= (- -1 ml) cml)))
              binding
              (make-binding 'out-of-phase #f)))
        binding)))

(define (bind! lbl b)
  (when lbl
    (store-set! (current-store) lbl b)))

(define (bind-core! lbl b)
  (store-set! *core-store* lbl b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Current Global Store ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (set-property! lbl val)
  (list-set! (binding-value (lookup lbl)) 1 val))

(define (arguments->vector id n producer)
  (define l (if (<= 0 n) n (- -1 n)))
  (define res (make-vector l))
  (receive args (producer)
    (define k (length args))
    (unless (or (and (<= 0 n) (= k n))
                (and (< n 0) (<= (- -2 n) k)))
      (raise-syntax-error id "wrong number of arguments"))
    (do ((i 0 (+ i 1))
         (args args (cdr args)))
        ((= i l) res)
      (vector-set! res i
                   (if (and (< n 0) (= i (- l 1)))
                       args
                       (car args))))))
