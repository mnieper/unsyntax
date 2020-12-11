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

(library (rnrs records syntactic (6))
  (export define-record-type
          fields mutable immutable parent protocol sealed opaque nongenerative
          parent-rtd
          record-type-descriptor
          record-constructor-descriptor)
  (import (except (scheme base) define-record-type)
          (rnrs records)
          (srfi :1)
          (srfi :8)
          (srfi :27)
          (srfi :206)
          (srfi :211 identifier-syntax)
          (srfi :211 syntax-case)
          (srfi :213)
          (unsyntax symbol)
          (unsyntax uuid))

  ;;;;;;;;;;;;;;;;;;;;;;
  ;; Auxiliary Syntax ;;
  ;;;;;;;;;;;;;;;;;;;;;;

  (define-auxiliary-syntax fields fields)
  (define-auxiliary-syntax mutable mutable)
  (define-auxiliary-syntax immutable immutable)
  (define-auxiliary-syntax parent parent)
  (define-auxiliary-syntax protocol protocol)
  (define-auxiliary-syntax sealed sealed)
  (define-auxiliary-syntax opaque opaque)
  (define-auxiliary-syntax nongenerative nongenerative)
  (define-auxiliary-syntax parent-rtd parent-rtd)

  ;;;;;;;;;;;;;;;;;;;
  ;; Property Keys ;;
  ;;;;;;;;;;;;;;;;;;;

  (define-syntax rtd-key
    (lambda (stx)
      (syntax-violation #f "invalid use of syntax" stx)))

  (define-syntax rcd-key
    (lambda (stx)
      (syntax-violation #f "invalid use of syntax" stx)))

  ;;;;;;;;;;;;
  ;; Syntax ;;
  ;;;;;;;;;;;;

  (define-syntax define-record-type
    (let* ((s (make-random-source))
           (random-uuid (begin (random-source-randomize! s)
                               (random-source-make-uuids s))))
      (lambda (stx)
        (syntax-case stx ()
          ((k name-spec record-clause* ...)
           (with-syntax
               (((record-name constructor-name predicate-name)
                 (syntax-case #'name-spec ()
                   ((record-name constructor-name predicate-name)
                    (and (identifier? #'record-name)
                         (identifier? #'constructor-name)
                         (identifier? #'predicate-name))
                    (list #'record-name #'constructor-name #'predicate-name))
                   (record-name
                    (identifier? #'record-name)
                    (let ((name (syntax->datum #'record-name)))
                      (list #'record-name
                            (datum->syntax #'k (symbol-append 'make- name))
                            (datum->syntax #'k (symbol-append name '?)))))
                   (_
                    (syntax-violation
                     'define-record-type "ill-formed name spec"
                     #'name-spec)))))
             (define genuid
               (let ((prefix (syntax->datum #'record-name)))
                 (lambda ()
                   (symbol-append
                    prefix '|-|
                    (string->symbol (uuid->string (random-uuid)))))))
             (define (parse-record-clause record-clause)
               (syntax-case record-clause (fields
                                           parent
                                           protocol
                                           sealed
                                           opaque
                                           nongenerative
                                           parent-rtd)
                 ((fields field-spec* ...)
                  (cons 'fields (map parse-field-spec #'(field-spec* ...))))
                 ((parent parent-name)
                  (identifier? #'parent-name)
                  (list 'parent #'parent-name))
                 ((protocol expression)
                  (list 'protocol #'expression))
                 ((sealed #t) (list 'sealed #t))
                 ((sealed #f) (list 'sealed #f))
                 ((opaque #t) (list 'opaque #t))
                 ((opaque #f) (list 'opaque #f))
                 ((nongenerative)
                  (list 'nongenerative (datum->syntax #'k (genuid))))
                 ((nongenerative uid)
                  (identifier? #'uid)
                  (list 'nongenerative #'uid))
                 ((parent-rtd %parent-rtd parent-cd)
                  (list 'parent-rtd #'%parent-rtd #'parent-cd))
                 (_
                  (syntax-violation
                   'define-record-type "invalid record clause"
                   record-clause))))
             (define (parse-field-spec field-spec)
               (define (accessor-name field-name)
                 (datum->syntax #'k
                                (symbol-append (syntax->datum #'record-name)
                                               '|-|
                                               (syntax->datum field-name))))
               (define (mutator-name field-name)
                 (datum->syntax #'k
                                (symbol-append (syntax->datum #'record-name)
                                               '|-|
                                               (syntax->datum field-name)
                                               '|-set!|)))
               (syntax-case field-spec (mutable immutable)
                 ((immutable field-name accessor-name)
                  (and (identifier? #'field-name)
                       (identifier? #'accessor-name))
                  (list #'immutable #'field-name #'accessor-name))
                 ((mutable field-name accessor-name mutator-name)
                  (and (identifier? #'field-name)
                       (identifier? #'accessor-name)
                       (identifier? #'mutator-name))
                  (list #'mutable #'field-name #'accessor-name
                        #'mutator-name))
                 ((immutable field-name)
                  (identifier? #'field-name)
                  (list #'immutable #'field-name
                        (accessor-name #'field-name)))
                 ((mutable field-name)
                  (identifier? #'field-name)
                  (list #'mutable #'field-name (accessor-name #'field-name)
                        (mutator-name #'field-name)))
                 (field-name
                  (identifier? #'field-name)
                  (list #'immutable #'field-name (accessor-name #'field-name)))
                 (_
                  (syntax-violation 'define-record-type
                                    "invalid field spec"
                                    field-spec))))
             (define specs
               (fold (lambda (record-clause specs)
                       (define spec
                         (parse-record-clause record-clause))
                       (when (assq (car spec) specs)
                         (syntax-violation
                          'define-record-type
                          "multiple record clause of the same kind"
                          record-clause))
                       (cons spec specs))
                     '() #'(record-clause* ...)))
             (define fields
               (list->vector (cond ((assq 'fields specs) => cdr)
                                   (else '()))))
             (define field-count (vector-length fields))
             (with-syntax
                 (((parent-rtd parent-cd)
                   (cond ((assq 'parent-rtd specs) =>
                          (lambda (spec)
                            (when (assq 'parent specs)
                              (syntax-violation
                               'define-record-type
                               "multiple parent/parent-rtd clause" stx))
                            (cdr spec)))
                         ((assq 'parent specs) =>
                          (lambda (spec)
                            (with-syntax ((parent-name (second spec)))
                              (list
                               #'(record-type-descriptor parent-name)
                               #'(record-constructor-descriptor
                                  parent-name)))))
                         (else
                          (list #f #f))))
                  (protocol (cond ((assq 'protocol specs) => second)
                                  (else #f)))
                  (uid (cond ((assq 'nongenerative specs) => second)
                             (else #f)))
                  (sealed? (cond ((assq 'sealed specs) => second)
                                 (else #f)))
                  (opaque? (cond ((assq 'opaque specs) => second)
                                 (else #f)))
                  (fields
                   (vector-map (lambda (field-spec)
                                 (list (first field-spec)
                                       (second field-spec)))
                               fields))
                  ((accessor* ...)
                   (let f ((i 0))
                     (if (= i field-count)
                         '()
                         (cons
                          (with-syntax ((accessor-name
                                         (third (vector-ref fields i)))
                                        (i i))
                            #'(define accessor-name (record-accessor rtd i)))
                          (f (+ i 1))))))
                  ((mutator* ...)
                   (let f ((i 0))
                     (cond
                      ((= i field-count)
                       '())
                      ((= 4 (length (vector-ref fields i)))
                       (cons
                        (with-syntax ((mutator-name
                                       (fourth (vector-ref fields i)))
                                      (i i))
                          #'(define mutator-name (record-mutator rtd i)))
                        (f (+ i 1))))
                      (else
                       (f (+ i 1)))))))
               #'(begin
                   (define-syntax record-name
                     (lambda (stx)
                       (syntax-violation 'define-record-type
                                         "invalid use of record name"
                                         stx)))
                   (define rtd
                     (make-record-type-descriptor 'record-name parent-rtd 'uid
                                                  sealed? opaque? 'fields))
                   (define rcd
                     (make-record-constructor-descriptor rtd parent-cd
                                                         protocol))
                   (define-property record-name rtd-key #'rtd)
                   (define-property record-name rcd-key #'rcd)
                   (define constructor-name
                     (record-constructor rcd))
                   (define predicate-name
                     (record-predicate rtd))
                   accessor* ...
                   mutator* ...))))))))

  (define-syntax record-type-descriptor
    (lambda (stx)
      (lambda (p)
        (syntax-case stx ()
          ((_ record-name)
           (p #'record-name #'rtd-key))))))

  (define-syntax record-constructor-descriptor
    (lambda (stx)
      (lambda (p)
        (syntax-case stx ()
          ((_ record-name)
           (p #'record-name #'rcd-key)))))))
