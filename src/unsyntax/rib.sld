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

(define-library (unsyntax rib)
  (export make-label
          label?
          make-label/props
          label/props-label
          label/props-props
          label/props-add
          label=?
          label-comparator
          make-rib
          make-mutable-rib
          rib-mutable?
          rib-add-barrier!
          rib-ref
          rib-ref/props
          rib-set!
          rib-set!/props
          rib->datum
          current-global-resolver
          resolve
          resolve/props
          resolve-prop
          free-identifier=?)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 1)
          (srfi 2)
          (srfi 8)
          (srfi 128)
          (unsyntax error)
          (unsyntax gensym)
          (unsyntax identifier)
          (unsyntax syntax))
  (include "rib.scm"))
