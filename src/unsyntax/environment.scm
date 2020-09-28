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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Substitution Environments ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define environment-mutable? car)
(define (environment-table env)
  (if (hash-table? (cdr env))
      (cdr env)
      (let ((ht (make-hash-table eq-comparator)))
        (for-each (lambda (entry)
                    (%environment-set!/props ht (caar entry) (cdar entry)
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

(define (environment-ref/props env id)
 (%environment-ref/props env (identifier-name id) (syntax-object-marks id)))

(define (environment-ref env id)
  (and-let* ((l/p (environment-ref/props env id)))
    (label/props-label l/p)))

(define (%environment-ref/props env name m*)
  (hash-table-ref (environment-table env)
                  name
                  (lambda ()
                    #f)
                  (lambda (val)
                    (and-let* ((entry (assoc m* val marks=?)))
                      (cdr entry)))))

(define (%environment-ref env name m*)
  (and-let* ((l/p (%environment-ref/props env name m*)))
    (label/props-label l/p)))

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

(define (%environment-set!/props ht name m* l/p)
  (%environment-update! ht name m* values
                        (lambda () l/p)
                        (lambda (prev-l/p) l/p)))

(define (environment-update! env id update failure success)
  (let ((name (identifier-name id))
        (m* (syntax-object-marks id)))
    (%environment-update! (environment-table env) name m*
                          update failure success)))

(define (environment-set!/props env id l/p)
  (environment-update! env
                       id
                       values
                       (lambda () l/p)
                       (lambda (prev-l/p)
                         (unless (or (environment-mutable? env)
                                     (label=? (label/props-label l/p)
                                              (label/props-label prev-l/p)))
                           (raise-syntax-error
                            id
                            "duplicate definition of identifier ‘~a’"
                            (identifier-name id)))
                         l/p)))

(define (environment-set! env id lbl)
  (environment-set!/props env id (make-label/props lbl '())))

(define (%environment-fold proc seed env)
  (hash-table-fold
   (lambda (name val acc)
     (fold (lambda (entry acc)
             (proc name (car entry) (cdr entry) acc))
           acc val))
   seed (environment-table env)))

(define (environment-fold proc seed env)
  (%environment-fold
   (lambda (name m* l/p acc)
     (proc (make-identifier name m*) (label/props-label l/p) acc))
   seed env))

(define (environment-fold/props proc seed env)
  (%environment-fold
   (lambda (name m* l/p acc)
     (proc (make-identifier name m*) l/p acc))
   seed env))

(define (environment-for-each proc env)
  (hash-table-for-each
   (lambda (name val)
     (for-each (lambda (entry)
                 (proc (make-identifier name (car entry)) (cdr entry)))
               val))
   (environment-table env)))

(define (environment->alist env label-present?)
  (%environment-fold
   (lambda (name m* l/p res)
     (if (label-present? (label/props-label l/p))
         (alist-cons (cons name m*) (label/props-filter label-present? l/p) res)
         res))
   '() env))

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
                   (or (%environment-ref/props (car s*) name m*)
                       (f m* (cdr s*)))))))
      ((current-global-resolver) id)))

(define (resolve-prop id id-lbl key-lbl)
  (or (let ((name (identifier-name id)))
        (let f ((m* (syntax-object-marks id))
                (s* (syntax-object-substs id)))
          (and (not (null? s*))
               (if (shift? (car s*))
                   (f (cdr m*) (cdr s*))
                   (ref-prop (%environment-ref/props (car s*) name m*)
                             id-lbl
                             key-lbl
                             (lambda ()
                               (f m* (cdr s*))))))))
      (ref-prop ((current-global-resolver) id) id-lbl key-lbl
                (lambda () #f))))

(define (free-identifier=? id1 id2)
  (let ((l1 (resolve id1))
        (l2 (resolve id2)))
    (if (or l1 l2)
        (label=? l1 l2)
        (symbol=? (identifier-name id1) (identifier-name id2)))))
