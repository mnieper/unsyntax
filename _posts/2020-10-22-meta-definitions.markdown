---
layout: post
title:  "Meta Definitions"
date:   2020-10-22 13:00:00 +0200
categories: unsyntax features syntax meta phasing
---
Everyone who has written procedural macros has probably stumbled over
phasing issues at least once:

~~~
$ cat prog1.scm
(import (scheme base)
        (srfi 211 syntax-case))

(define (quote-syntax x)
  #`(quote #,(datum->syntax #'* x)))

(define-syntax quote-foo
  (lambda (x)
    (quote-syntax 'foo)))
$ unsyntax-scheme prog1.scm
prog1.scm: 9.6-17: out of phase identifier ‘quote-syntax’
~~~

The reason for this is that the definition of `quote-syntax` creates a
runtime binding, which is not established until the program is
executed.  The right hand side of the syntax definition of
`quote-foo`, however, is already evaluated at expand-time when the
`quote-syntax` procedure does not exists yet.

There are a number of solutions.  The obvious one is to move the
definition of `quote-syntax` to the right hand side of a syntax
definition so that it is already evaluated at expand time:

~~~ scheme
(import (scheme base)
        (srfi 211 syntax-case))

(define-syntax quote-foo
  (lambda (x)
    (define (quote-syntax x)
      #`(quote #,(datum->syntax #'* x)))
    (quote-syntax 'foo)))
~~~

The disadvantage of this approach (aside from the extra indentation of
`quote-syntax`) is that it hardly generalizes if more than one macro
wants to use the helper procedure.

A viable approach is to create a helper library that contains the
helper procedure:

~~~ scheme
(library (quote-syntax)
  (export quote-syntax)
  (import (scheme base)
          (srfi 211 syntax-case))
  (define (quote-syntax x)
    #`(quote #,(datum->syntax #'* x))))
~~~

The top-level program then looks like this:

~~~ scheme
(import (scheme base)
        (quote-syntax))

(define-syntax quote-foo
  (lambda (x)
    (quote-syntax 'foo)))
~~~

This time, we don't get a phasing error.  The reason is that a library
can be evaluated already at expand-time of another library or the
top-level program.  While this is a working solution, it creates the
need of an extra helper library, which can be undesirable (especially
if it would lead to a chain of dependent helper libraries).

So let's try again with a one-file solution.  We can try to change the
variable definition of `quote-syntax` into a syntax definition:

~~~ scheme
(import (scheme base)
        (srfi 211 syntax-case))

(define-syntax quote-syntax
  (syntax-rules
    ((quote-syntax x) #`(quote #,(datum->syntax #'* x)))))

(define-syntax quote-foo
  (lambda (x)
    (quote-syntax 'foo)))
~~~

This works because the syntax binding to `quote-syntax` here is
already established at expand-time.  It is not an approach that
scales, though, as the `quote-syntax` macro will inline its template
everywhere it is used, which becomes unviable when the helper
"procedures" become large.

All solutions presented so far had their flaws or were, comparably,
complicated.  So let's think of a simple solution once more.  The
original problem was that the variable binding of `quote-syntax` was
established at runtime and not at expand-time.  Wouldn't it be nice if
we could tell our Scheme system that we want this definition to take
place at expand-time and not later?

And, in fact, this is now possible with Unsyntax, which now supports
the `meta` keyword possibly known from Chez Scheme.  Any form prefixed
with meta becomes a definition, which is evaluated at expand-time
instead of runtime.  And so our final solution looks as follows (the
`meta` keyword is exported by the `(unsyntax)` library):

~~~ scheme
(import (scheme base)
        (srfi 211 syntax-case)
        (unsyntax))

(meta define (quote-syntax x)
  #`(quote #,(datum->syntax #'* x)))

(define-syntax quote-foo
  (lambda (x)
    (quote-syntax 'foo)))
~~~
