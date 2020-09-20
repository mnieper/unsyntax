---
layout: page
title: Download
permalink: /download/
---

There is not yet a stable release of Unsyntax.  You can, however,
download the latest development version as a [tarball][unsyntax-tb].

After you have unpacked the tarball, you can do the usual

~~~
$ ./configure
$ ./make
$ ./sudo make install
~~~

Afterwards, you should be able to run the Unsyntax interpreter with
the command

~~~
$ unsyntax-scheme [options] [--] [program [arg]...]
~~~

and the compiler with the command

~~~
$ compile-unsyntax [options] file
~~~

See the file INSTALL for generic compilation and installation instructions.

Unsyntax requires (the development version of) Chibi-Scheme to build and
run.

[unsyntax-tb]: https://gitlab.com/nieper/unsyntax/-/jobs/artifacts/master/raw/build/unsyntax-latest.tar.gz?job=build-distcheck
