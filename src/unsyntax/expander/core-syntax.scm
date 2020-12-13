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

(define-core-syntax! 'alias)
(define-core-syntax! 'auxiliary-syntax-name)
(define-core-syntax! 'begin)
(define-core-syntax! 'define-auxiliary-syntax)
(define-core-syntax! 'define-property)
(define-core-syntax! 'define-record-type)
(define-core-syntax! 'define-syntax)
(define-core-syntax! 'define-syntax-parameter)
(define-core-syntax! 'define-values)
(define-core-syntax! 'import)
(define-core-syntax! 'import-only)
(define-core-syntax! 'meta 'meta-form)
(define-core-syntax! 'module 'module-form)
(define-core-syntax! 'splicing-let-syntax 'let-syntax)
(define-core-syntax! 'splicing-letrec-syntax 'letrec-syntax)
(define-core-syntax! 'with-ellipsis)
