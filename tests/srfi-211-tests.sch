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
        (scheme cxr)
        (srfi 64)
        (rename (srfi 211 explicit-renaming)
                (identifier? er-identifier?))
        (srfi 211 implicit-renaming)
        (rename (srfi 211 syntactic-closures)
                (identifier? sc-identifier?))
        (srfi 212))

(test-begin "SRFI 211")

(test-group "ER Macros"
  (define-syntax loop
    (er-macro-transformer
      (lambda (x r c)
        (let ((body (cdr x)))
          `(,(r 'call-with-current-continuation)
            (,(r 'lambda) (exit)
             (,(r 'let) ,(r 'f) () ,@body (,(r 'f)))))))))

  (test-equal 42 (loop (exit 42))))

(test-group "IR Macros"
  (define-syntax loop
    (ir-macro-transformer
     (lambda (expr inject compare)
       (let ((body (cdr expr)))
         `(call-with-current-continuation
           (lambda (,(inject 'exit))
             (let f () ,@body (f))))))))

  (test-equal 42 (loop (exit 42))))

(test-group "sc-macro-transformer"
  (define-syntax push
    (sc-macro-transformer
     (lambda (exp env)
       (let ((item (make-syntactic-closure env '() (cadr exp)))
             (list (make-syntactic-closure env '() (caddr exp))))
         `(set! ,list (cons ,item ,list))))))

  (define-syntax loop
    (sc-macro-transformer
     (lambda (exp env)
       (let ((body (cdr exp)))
         `(call-with-current-continuation
           (lambda (exit)
             (let f ()
               ,@(map (lambda (exp)
                        (make-syntactic-closure env '(exit)
                                                exp))
                      body)
               (f))))))))


  (test-equal '(2 1) (let ((x '(1)))
                       (push 2 x)
                       x))

  (test-equal 'sc-loop (loop (exit 'sc-loop))))

(test-group "rsc-macro-transformer"
  (define-syntax push
    (rsc-macro-transformer
     (lambda (exp env)
       `(,(make-syntactic-closure env '() 'set!)
         ,(caddr exp)
         (,(make-syntactic-closure env '() 'cons)
          ,(cadr exp)
          ,(caddr exp))))))

  (test-equal '(4 3) (let ((x '(3)))
                       (push 4 x)
                       x)))

(test-group "capture-syntactic-environment"
  (define-syntax loop-until
    (sc-macro-transformer
     (lambda (exp env)
       (let ((id (cadr exp))
             (init (caddr exp))
             (test (cadddr exp))
             (return (cadddr (cdr exp)))
             (step (cadddr (cddr exp)))
             (close
              (lambda (exp free)
                (make-syntactic-closure env free exp))))
         `(letrec ((loop
                    ,(capture-syntactic-environment
                      (lambda (env)
                        `(lambda (,id)
                           (,(make-syntactic-closure env '() 'if)
                            ,(close test (list id))
                            ,(close return (list id))
                            (,(make-syntactic-closure env '() 'loop)
                             ,(close step (list id)))))))))
            (loop ,(close init '())))))))

  (test-equal 1 (loop-until x 0 (= x 1) x (+ x 1))))

(test-group "make-syntactic-closure"
  (define-syntax let1
    (sc-macro-transformer
     (lambda (exp env)
       (let ((id (cadr exp))
             (init (caddr exp))
             (exp (cadddr exp)))
         `((lambda (,id)
             ,(make-syntactic-closure env (list id) exp))
           ,(make-syntactic-closure env '() init))))))

  (test-equal 'let1 (let1 x 'let1 x)))

(test-group "Identifiers"
  (test-equal '(#t #f)
    (let-syntax
        ((foo
          (sc-macro-transformer
           (lambda (form env)
             (capture-syntactic-environment
              (lambda (transformer-env)
                (identifier=? transformer-env 'x env 'x)))))))
      (list (foo)
            (let ((x 3))
              (foo)))))

  (test-equal '(#f #t)
    (let ((foo 'foo))
      (alias bar foo)
      (let-syntax
          ((foo
            (sc-macro-transformer
             (lambda (form env)
               (capture-syntactic-environment
                (lambda (transformer-env)
                  (identifier=? transformer-env 'foo
                                env (cadr form))))))))
        (list (foo foo)
              (foo bar))))))

(test-end)
