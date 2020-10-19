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

(define (random-source-make-uuids s)
  (define random-integer (random-source-make-integers s))
  (lambda ()
    (define uuid (make-bytevector 16))
    (do ((i 0 (+ i 1)))
        ((= i 16) uuid)
      (bytevector-u8-set! uuid
                          i
                          (case i
                            ((6)
                             (+ #x40 (random-integer #x10)))
                            ((8)
                             (+ #x80 (random-integer #x40)))
                            (else
                             (random-integer #x100)))))))

(define random-uuid
  (random-source-make-uuids default-random-source))

(define (uuid->string uuid)
  (apply format "~a~a~a~a-~a~a-~a~a-~a~a-~a~a~a~a~a~a"
         (unfold (lambda (i) (= i 16))
                 (lambda (i)
                   (define u8 (bytevector-u8-ref uuid i))
                   (define-values (high low) (truncate/ u8 16))
                   (string (hex-digit high) (hex-digit low)))
                 (lambda (i) (+ i 1))
                 0)))

(define (hex-digit n)
  (integer->char (+ n (if (<= 0 n 9) #x30 #x57))))
