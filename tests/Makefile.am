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

scheme_tests = r7rs-tests.sch syntax-case-tests.sch srfi-1-tests.sch	\
               srfi-2-tests.sch srfi-46-tests.sch srfi-188-tests.sch	\
               srfi-190-tests.sch
compiler_tests = test-program.test

wrapper = $(chibi_scheme) -h4G -A$(top_builddir)/src	\
          -A$(top_srcdir)/src --
unsyntax_scheme = $(top_builddir)/bin/unsyntax-scheme
expand_unsyntax = $(top_builddir)/bin/expand-unsyntax

TESTS = $(compiler_tests) $(scheme_tests)
EXTRA_DIST = $(scheme_tests) test-program.scm example/library.sld

TEST_EXTENSIONS = .test .sch
LOG_DRIVER = env AM_TAP_AWK='$(AWK)' $(SHELL)		\
             $(top_srcdir)/build-aux/tap-driver.sh
SCH_LOG_DRIVER = $(LOG_DRIVER)
SCH_LOG_COMPILER = $(wrapper) $(unsyntax_scheme) -A $(builddir) -A	\
                   $(srcdir)
TEST_LOG_DRIVER = $(LOG_DRIVER)
TEST_LOG_COMPILER = $(wrapper)

.scm.test:
	$(AM_V_GEN)$(wrapper) $(expand_unsyntax) -D test -A	\
	$(builddir) -A $(srcdir) -o $@ $<
