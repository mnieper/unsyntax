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

(define-library (srfi 64)
  (export test-begin
	  test-end
	  test-assert
	  test-eqv
	  test-eq
	  test-equal
	  test-approximate
	  test-error
	  test-apply
	  test-with-runner
	  test-match-nth
	  test-match-all
	  test-match-any
	  test-match-name
	  test-skip
	  test-expect-fail
	  test-read-eval-string
	  test-runner-group-path
	  test-group
	  test-group-with-cleanup
	  test-result-ref
	  test-result-set!
	  test-result-clear
	  test-result-remove
	  test-result-kind
	  test-passed?
	  test-runner?
	  test-runner-reset
	  test-runner-null
	  test-runner-simple
	  test-runner-current
	  test-runner-factory
	  test-runner-get
	  test-runner-create
	  test-runner-test-name
	  test-runner-pass-count
	  test-runner-fail-count
	  test-runner-xpass-count
	  test-runner-xfail-count
	  test-runner-skip-count
	  test-runner-group-stack
	  test-runner-on-test-begin test-runner-on-test-begin!
	  test-runner-on-test-end test-runner-on-test-end!
	  test-runner-on-group-begin test-runner-on-group-begin!
	  test-runner-on-group-end test-runner-on-group-end!
	  test-runner-on-final test-runner-on-final!
	  test-runner-on-bad-count test-runner-on-bad-count!
	  test-runner-on-bad-end-name test-runner-on-bad-end-name!
	  test-result-alist
	  test-runner-aux-value test-runner-aux-value!
	  test-on-test-end-simple
	  test-on-group-begin-simple
	  test-on-group-end-simple
	  test-on-bad-count-simple
	  test-on-bad-end-name-simple
	  test-on-final-simple)
    (import (scheme base)
	    (scheme case-lambda)
	    (scheme eval)
	    (scheme read)
	    (scheme repl)
	    (scheme write))
    (include "64.scm"))