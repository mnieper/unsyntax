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
;; Labels ;;
;;;;;;;;;;;;

(define (make-label)
  (gensym "l"))

(define label=? equal?)
(define label-comparator eq-comparator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Substitution Environments ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define environment-mutable? car)
(define (environment-table env)
  (if (hash-table? (cdr env))
      (cdr env)
      (let ((ht (make-hash-table eq-comparator)))
        (for-each (lambda (entry)
                    (%environment-set! ht (caar entry) (cdar entry)
                                       (cdr entry)))
                  (cdr env))
        (set-cdr! env ht)
        ht)))

(define make-environment
  (case-lambda
    (()
     (cons #f (make-hash-table eq-comparator)))
    ((id* lbl*)
     (let ((name* (map identifier-name id*))
           (m** (map syntax-object-marks id*))
           (env (make-environment)))
       (for-each (lambda (id lbl)
                   (environment-set! env id lbl))
                 id* lbl*)
       env))))

(define (make-mutable-environment)
  (cons #t (make-hash-table eq-comparator)))

(define (environment-ref env id)
  (%environment-ref env (identifier-name id) (syntax-object-marks id)))

(define (%environment-ref env name m*)
  (hash-table-ref (environment-table env)
                  name
                  (lambda ()
                    #f)
                  (lambda (val)
                    (and-let* ((entry (assoc m* val marks=?)))
                      (cdr entry)))))

(define (%environment-update! ht name m* update failure success)
  (hash-table-update!/default
   ht
   name
   (lambda (val)
     (cond ((assoc m* val marks=?)
            => (lambda (entry)
                 (set-cdr! entry (update (success (cdr entry))))
                 val))
           (else
            (alist-cons m* (update (failure)) val))))
   '()))

(define (%environment-set! ht name m* lbl)
  (%environment-update! ht name m* values
                        (lambda () lbl)
                        (lambda (prev-lbl) lbl)))

(define (environment-update! env id update failure success)
  (let ((name (identifier-name id))
        (m* (syntax-object-marks id)))
    (%environment-update! (environment-table env) name m*
                          update failure success)))

(define (environment-set! env id lbl)
  (environment-update! env
                       id
                       values
                       (lambda () lbl)
                       (lambda (prev-lbl)
                         (unless (or (environment-mutable? env)
                                     (label=? lbl prev-lbl))
                           (raise-syntax-error
                            id
                            "duplicate definition of identifier ‘~a’"
                            (identifier-name id)))
                         lbl)))

(define (%environment-fold proc seed env)
  (hash-table-fold
   (lambda (name val acc)
     (fold (lambda (entry acc)
             (proc name (car entry) (cdr entry) acc))
           acc val))
   seed (environment-table env)))

(define (environment-fold proc seed env)
  (%environment-fold (lambda (name m* lbl acc)
		       (proc (make-identifier name m*) lbl acc))
		     seed env))

(define (environment-for-each proc env)
  (hash-table-for-each
   (lambda (name val)
     (for-each (lambda (entry)
                 (proc (make-identifier name (car entry)) (cdr entry)))
               val))
   (environment-table env)))

(define (environment->alist env label-present?)
  (%environment-fold (lambda (name m* lbl res)
                       (if (label-present? lbl)
                           (alist-cons (cons name m*) lbl res)
                           res))
		     '() env))

;;;;;;;;;;;;;;;;;;;;;;
;; Resolving labels ;;
;;;;;;;;;;;;;;;;;;;;;;

(define current-global-resolver
  (make-parameter (lambda (id) #f)))

(define (resolve id)
  (or (let ((name (identifier-name id)))
        (let f ((m* (syntax-object-marks id))
                (s* (syntax-object-substs id)))
          (and (not (null? s*))
               (if (shift? (car s*))
                   (f (cdr m*) (cdr s*))
                   (or (%environment-ref (car s*) name m*)
                       (f m* (cdr s*)))))))
      ((current-global-resolver) id)))


(define (free-identifier=? id1 id2)
  (let ((l1 (resolve id1))
        (l2 (resolve id2)))
    (if (or l1 l2)
        (label=? l1 l2)
        (symbol=? (identifier-name id1) (identifier-name id2)))))
