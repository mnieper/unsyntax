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

(define (format format-string . objects)
  (let ((buffer (open-output-string)))
    (let f ((format-list (string->list format-string))
               (objects objects))
      (cond
       ((null? format-list)
	(get-output-string buffer))
       ((char=? (car format-list) #\~)
        (case (cadr format-list)
	  ((#\a)
	   (display (car objects) buffer)
	   (f (cddr format-list) (cdr objects)))
	  ((#\s)
	    (write (car objects) buffer)
	    (f (cddr format-list) (cdr objects)))
	  ((#\%)
	   (newline buffer)
	   (f (cddr format-list) objects))
	  ((#\~)
	   (write-char #\~ buffer)
	   (f (cddr format-list) objects))))
       (else
        (write-char (car format-list) buffer)
        (f (cdr format-list) objects))))))
