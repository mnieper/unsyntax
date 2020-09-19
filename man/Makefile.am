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

if !GIT_CROSS_COMPILING
dist_man_MANS = unsyntax-scheme.1 expand-unsyntax.1 compile-unsyntax.1
endif

if BUILD_FROM_GIT
noinst_SCRIPTS = unsyntax-scheme expand-unsyntax
CLEANFILES = $(noinst_bin_SCRIPTS)
MAINTAINERCLEANFILES = $(dist_man_MANS)

do_subst = $(SED) -e 's,[@]CHIBI_SCHEME[@],$(chibi_scheme),g'	\
                  -e 's,[@]TOP_BUILDDIR[@],$(top_builddir),g'	\
                  -e 's,[@]TOP_SRCDIR[@],$(top_srcdir),g'

unsyntax-scheme.1: ../src/unsyntax/scheme.scm				\
                   ../src/unsyntax/version-etc.scm			\
                   unsyntax-scheme.h2m $(top_srcdir)/configure.ac	\
                   unsyntax-scheme.in
	$(AM_V_at)$(MAKE) unsyntax-scheme
	$(AM_V_GEN)$(HELP2MAN) --include=$(srcdir)/unsyntax-scheme.h2m	\
	-L C.UTF-8 -o $@-t $(builddir)/unsyntax-scheme
	$(AM_V_at)chmod a=r $@-t
	$(AM_V_at)mv -f $@-t $@

expand-unsyntax.1: ../src/unsyntax/scheme.scm				\
                   ../src/unsyntax/version-etc.scm			\
                   expand-unsyntax.h2m $(top_srcdir)/configure.ac	\
                   expand-unsyntax.in
	$(AM_V_at)$(MAKE) expand-unsyntax
	$(AM_V_GEN)$(HELP2MAN) --include=$(srcdir)/expand-unsyntax.h2m	\
	-L C.UTF-8 -o $@-t $(builddir)/expand-unsyntax
	$(AM_V_at)chmod a=r $@-t
	$(AM_V_at)mv -f $@-t $@

compile-unsyntax.1: ../bin/compile-unsyntax.in				\
                   compile-unsyntax.h2m $(top_srcdir)/configure.ac
	$(AM_V_GEN)$(HELP2MAN) --include=$(srcdir)/compile-unsyntax.h2m	\
	-L C.UTF-8 -o $@-t -h -h -v -v ../bin/compile-unsyntax
	$(AM_V_at)chmod a=r $@-t
	$(AM_V_at)mv -f $@-t $@

unsyntax-scheme: unsyntax-scheme.in
	$(AM_V_GEN)$(do_subst) < $(srcdir)/unsyntax-scheme.in > $@
	$(AM_V_at)chmod +x $@

expand-unsyntax: expand-unsyntax.in
	$(AM_V_GEN)$(do_subst) < $(srcdir)/expand-unsyntax.in > $@
	$(AM_V_at)chmod +x $@

endif
