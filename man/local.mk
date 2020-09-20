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
dist_man_MANS += %D%/unsyntax-scheme.1 %D%/expand-unsyntax.1	\
%D%/compile-unsyntax.1
endif

if BUILD_FROM_GIT
noinst_SCRIPTS += %D%/unsyntax-scheme %D%/expand-unsyntax
CLEANFILES += $(noinst_SCRIPTS)
MAINTAINERCLEANFILES += $(dist_man_MANS)

%D%/unsyntax-scheme.1: %D%/unsyntax-scheme.h2m %D%/unsyntax-scheme
	$(AM_V_GEN)$(HELP2MAN)						\
	--include=$(top_srcdir)/man/unsyntax-scheme.h2m -L C.UTF-8 -o	\
	$@-t %D%/unsyntax-scheme
	$(AM_V_at)chmod a=r $@-t
	$(AM_V_at)mv -f $@-t $@

%D%/expand-unsyntax.1: %D%/expand-unsyntax.h2m %D%/expand-unsyntax
	$(AM_V_GEN)$(HELP2MAN)						\
	--include=$(top_srcdir)/man/expand-unsyntax.h2m -L C.UTF-8 -o	\
	$@-t %D%/expand-unsyntax
	$(AM_V_at)chmod a=r $@-t
	$(AM_V_at)mv -f $@-t $@

%D%/compile-unsyntax.1: src/compile-unsyntax	\
%D%/compile-unsyntax.h2m
	$(AM_V_GEN)$(HELP2MAN)						\
	--include=$(top_srcdir)/man/compile-unsyntax.h2m -L C.UTF-8 -o	\
	$@-t -h -h -v -v $(top_builddir)/src/compile-unsyntax
	$(AM_V_at)chmod a=r $@-t
	$(AM_V_at)mv -f $@-t $@

%D%/unsyntax-scheme: %D%/unsyntax-scheme.in Makefile src/unsyntax-scheme
	$(AM_V_GEN)$(MKDIR_P) $(top_builddir)/man
	$(AM_V_at)$(do_subst) < $(top_srcdir)/man/unsyntax-scheme.in > $@
	$(AM_V_at)chmod +x $@

%D%/expand-unsyntax: %D%/expand-unsyntax.in Makefile src/expand-unsyntax
	$(AM_V_GEN)$(MKDIR_P) $(top_builddir)/man
	$(AM_V_at)$(do_subst) < $(top_srcdir)/man/expand-unsyntax.in > $@
	$(AM_V_at)chmod +x $@
endif
