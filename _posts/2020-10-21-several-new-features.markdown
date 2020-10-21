---
layout: post
title:  "Several new features"
date:   2020-10-21 22:00:00 +0200
categories: unsyntax features
---
It's time to talk about the progress Unsyntax has made during the last
four weeks.

* Two well-established SRFIs have been added.

  * SRFI 27

  * SRFI 111

* The brand new SRFI 213 uses Unsyntax as its sample implementation.

* Unsyntax is the right tool to experiment with various macro systems.
  It fully supports all systems described in SRFI 211:

  * The R4RS low-level system.

  * Explicitly renaming macros.

  * The syntax-case system.

  * Identifier syntax.

  * Variable transformers.

  * Custom ellipsis identifiers.

  * Syntax parameters.

  * Implicitly renaming macros.

  * Syntactic closures.

* Some R6RS support has been added to allow experimentation at to make
  it easier to import R6RS code into the R7RS world.

  * SRFIs can also be imported under their R6RS names (srfi :nnn).

  * The R6RS library syntax (including phasing and versioning) is
    supported alongside with the R7RS syntax.

  * The recognized extensions for libraries files are now
    ‘.unsyntax.sls’, ‘.sls’, and ‘.sld’.

  * The three R6RS record libraries have been implemented on top of
    the R7RS records.

The latest tarball is always [packaged and available for you to
download and install][unsyntax-tb].  Please file all bugs/feature
requests at Unsyntax's Gitlab repo or email them to
[bug@unsyntax.org][unsyntax-bug].

[unsyntax-tb]: https://gitlab.com/nieper/unsyntax/-/jobs/artifacts/master/raw/build/unsyntax-latest.tar.gz?job=build-distcheck
[unsyntax-bug]: mailto:bug@unsyntax.org
