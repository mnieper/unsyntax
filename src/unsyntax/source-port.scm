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

(define-record-type <source-port>
  (%make-source-port in ci? filename pos)
  source-port?
  (in source-port-in)
  (ci? source-port-ci? source-port-set-ci!)
  (filename source-port-filename)
  (pos source-port-position source-port-set-position!))

(define (make-source-port in ci? filename)
  (%make-source-port in ci? filename (position 1 0)))

(define (source-port-peek p)
  (peek-char (source-port-in p)))

(define (source-port-read p)
  (let ((ch (read-char (source-port-in p))))
    (unless (eof-object? ch)
      (let*
          ((pos (source-port-position p))
           (line (position-line pos))
           (pos
            (case ch
              ((#\newline #\x0a)
               (position (+ 1 line) 0))
              ((#\return)
               (position line 0))
              ((#\tab)
               (let ((col (position-column pos)))
                 (position line
                           (+ (- 8 (floor-remainder col 8)) col))))
              (else
               (let ((col (position-column pos)))
                 (position line (+ (wcwidth ch) col)))))))
        (source-port-set-position! p pos)))
    ch))
