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

(define the-known-features
  '(r7rs exact-closed exact-complex ieee-float full-unicode ratios posix windows
    unix darwin gnu-linux bsd freebsd solaris i386 x86-64 ppc sparc jvm clr llvm
    ilp32 lp64 ilp64 big-endian little-endian))

(define name (string-downcase (package-name)))

(define *features*
  (delay
    (lset-adjoin
     symbol=? (lset-intersection symbol=?
                                 the-known-features
                                 (host-features))
     (string->symbol name)
     (string->symbol (format "~a-~a" name (package-version))))))

(define current-features
  (case-lambda
    (()
     (features))
    ((features)
     (set! *features* (make-promise  (delete-duplicates! features symbol=?))))))

(define (features) (force *features*))
