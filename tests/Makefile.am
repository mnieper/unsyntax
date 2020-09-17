# Copyright © Marc Nieper-Wißkirchen (2020).

# This file is part of unsyntax.

# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:

# The above copyright notice and this permission notice (including the
# next paragraph) shall be included in all copies or substantial
# portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

scheme_tests = r7rs-tests.scm syntax-case-tests.scm srfi-1-tests.scm	\
               srfi-2-tests.scm srfi-46-tests.scm srfi-188-tests.scm	\
               srfi-190-tests.scm library-tests.scm

unsyntax_scheme = $(top_builddir)/bin/unsyntax-scheme

TESTS = $(scheme_tests)
EXTRA_DIST = $(scheme_tests) example/library.sld

TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = $(chibi_scheme) -h4G -A$(top_builddir)/src		\
                   -A$(top_srcdir)/src -- $(unsyntax_scheme)		\
                   -A $(builddir) -A $(srcdir)
SCM_LOG_DRIVER = env AM_TAP_AWK='$(AWK)' $(SHELL)	\
                 $(top_srcdir)/build-aux/tap-driver.sh
