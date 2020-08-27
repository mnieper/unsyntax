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

(define-library (srfi 125)
  (export make-hash-table
          hash-table
          hash-table-unfold
          alist->hash-table
          hash-table?
          hash-table-contains?
          hash-table-empty?
          hash-table=?
          hash-table-mutable?
          hash-table-ref
          hash-table-ref/default
          hash-table-set!
          hash-table-delete!
          hash-table-intern!
          hash-table-update!
          hash-table-update!/default
          hash-table-pop!
          hash-table-clear!
          hash-table-size
          hash-table-keys
          hash-table-values
          hash-table-entries
          hash-table-find
          hash-table-count
          hash-table-map
          hash-table-for-each
          hash-table-map!
          hash-table-map->list
          hash-table-fold
          hash-table-prune!
          hash-table-copy
          hash-table-empty-copy
          hash-table->alist
          hash-table-union!
          hash-table-intersection!
          hash-table-difference!
          hash-table-xor!)
  (import (unsyntax $bootstrap)))
