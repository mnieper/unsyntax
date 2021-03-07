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

(import (scheme base)
        (srfi 64)
        (srfi 188)
        (srfi 211 syntax-case)
        (srfi 212)
        (srfi 213))

(define-syntax get-property
  (lambda (stx)
    (capture-lookup
     (lambda (lookup)
       (syntax-case stx ()
         ((_ id key)
          #`'#,(datum->syntax #'*
                              (guard (c (else "error"))
                                (lookup #'id #'key)))))))))

(test-begin "SRFI 213")

(define info)
(define x "x-value")
(define-property x info "x-info")

(test-equal "x-value" x)
(test-equal "x-info" (get-property x info))

(test-equal #f (let ((x 0))
                 (get-property x info)))

(test-equal "x-new-info" (let* ()
                           (define-property x info "x-new-info")
                           (get-property x info)))

(test-assert
    (let ()
      (define y)
      (define-property x info #'id)
      (define-property y info #'id)
      (let-syntax ((get (lambda (stx)
                          (lambda (p)
                            (bound-identifier=? (p #'x #'info)
                                                (p #'y #'info))))))
        get)))

(test-equal "error" (get-property unbound-id info))

(test-group "Tests contributed by Jim Rees"

  (define-syntax identifier-property
    (lambda (stx)
      (capture-lookup
       (lambda (lookup)
         (syntax-case stx ()
           ((_ identifier key)
            (with-syntax ((value (lookup #'identifier #'key)))
              #'(quote value))))))))

  ;; Set up the top-level property.
  (define computer "the-computer")
  (define answer #f)
  (define-property computer answer (* 2 21))

  (test-equal 42 (identifier-property computer answer))

  ;; The second define-property overwrites the first one in a spliced context.
  (test-equal '(888 888)
    (let ()
      (define-property computer answer 999)
      (splicing-let-syntax ((identifier-property-alt
                             (syntax-rules ()
                               ((_) (identifier-property computer answer)))))
        (define-property computer answer 888)
        (list (identifier-property-alt)
              (identifier-property computer answer)))))

  ;; The macro picks up the outer definition.  The inner define-property does
  ;; not overwrite the property as it is in a different body.
  (test-equal
      '(888 42)
    (let-syntax ((m (syntax-rules ()
                      ((_)
                       (identifier-property computer answer)))))
      (define-property computer answer 888)
      (list (identifier-property computer answer)
            (m))))

  ;; Demonstrate that properties can be fetched at higher phases too
  (test-equal
      (* 42 42)
    (let ((r (identifier-property computer answer)))
      (splicing-let-syntax
          ((m (with-syntax ((v (identifier-property computer answer)))
                (syntax-rules ()
                  ((_ x)
                   (* x v))))))
        (m r))))

  ;; This demonstrates that property settings behave like macro
  ;; definitions - processed sequentially on the first pass through a
  ;; body until a non-definition is found.  The final property setting
  ;; is then in effect for the RHSs of variable definitions and
  ;; expressions on the second pass.
  (test-equal '(42 51 50 51)
    (let ((outer (identifier-property computer answer)))
      (define var0 (identifier-property computer answer))
      (define-property computer answer 50)
      (define-syntax m1 (lambda (stx)
                          (identifier-property computer answer)))
      (define-property computer answer 51)
      (define var2 (identifier-property computer answer))
      (list outer var0 (m1) var2)))

  ;; This demonstrates that the 'key' identifier can be "introduced"
  ;; by a macro and the property is still accessible by the calling
  ;; code:
  (test-equal 29
    (let ()
      (define-syntax m
        (syntax-rules ()
          ((_ id value)
           (define-property id answer value))))
      (m computer 29)
      (identifier-property computer answer)))

  ;; Alternatively, this demonstrates that when the id is "introduced"
  ;; by the macro the property is not accessible via the un-marked id.
  ;; The macro can provide a fetcher that passes back the
  ;; correctly-marked id.
  (test-equal '(42 29)
    (let ()
      (define-syntax m
        (syntax-rules ()
          ((_ value call-with-id)
           (begin
             (define-property computer answer value)
             (define-syntax call-with-id
               (syntax-rules ()
                 ((_ cb . other-args)
                  (cb computer . other-args))))))))
      (m 29 getprop)
      (list (identifier-property computer answer)
            (getprop identifier-property answer))))

  ;; Properties are attached to identifiers and not to bindings:
  (test-equal #f
    (let ()
      (alias amiga computer)
      (alias response answer)
      (identifier-property amiga response)))

  (test-equal '(1 42)
    (let ()
      (alias amiga computer)
      (alias response answer)
      (define-property amiga response 1)
      (list (identifier-property amiga answer)
            (identifier-property computer answer)))))

(test-end)
