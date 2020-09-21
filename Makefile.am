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

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = $(top_srcdir)/.prev-version $(top_srcdir)/.version
BUILT_SOURCES = $(top_srcdir)/.version
MOSTLYCLEANFILES =
CLEANFILES =
MAINTAINERCLEANFILES =
TESTS =
noinst_LIBRARIES =
bin_SCRIPTS =
noinst_SCRIPTS =
dist_man_MANS =
AM_INSTALLCHECK_STD_OPTIONS_EXEMPT =

scheme = env CHIBI_MODULE_PATH= $(chibi_scheme) -h2G		\
-A$(top_builddir)/src -A$(top_srcdir)/src $(SCHEME_FLAGS) --

do_subst = $(SED) -e 's,[@]PACKAGE_NAME[@],$(PACKAGE_NAME),g' \
            -e 's,[@]PACKAGE_BUGREPORT[@],$(PACKAGE_BUGREPORT),g' \
            -e 's,[@]PACKAGE_URL[@],$(PACKAGE_URL),g' \
            -e 's,[@]PACKAGE_VERSION[@],$(PACKAGE_VERSION),g' \
            -e 's,[@]BINDIR[@],$(bindir),g' \
            -e 's,[@]TOP_BUILDDIR[@],$(top_builddir),g' \
            -e 's,[@]TOP_SRCDIR[@],$(top_srcdir),g' \
            -e 's,[@]PKGDATADIR[@],$(pkgdatadir),g' \
            -e 's,[@]CHIBI_SCHEME[@],$(chibi_scheme),g'

include $(top_srcdir)/lib/local.mk
include $(top_srcdir)/src/local.mk
include $(top_srcdir)/doc/local.mk
include $(top_srcdir)/man/local.mk
include $(top_srcdir)/tests/local.mk

$(top_srcdir)/.version:
	$(AM_V_GEN)echo $(VERSION) > $@-t && mv $@-t $@
dist-hook: gen-ChangeLog
	$(AM_V_at)echo $(VERSION) > $(distdir)/.tarball-version
	$(AM_V_at)chmod u+w $(distdir)/$(man_MANS)

.PHONY: gen-ChangeLog
gen-ChangeLog:
	$(AM_V_GEN)if test -d .git; then		\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    > $(distdir)/cl-t &&			\
	  { rm -f $(distdir)/ChangeLog &&		\
	    mv $(distdir)/cl-t $(distdir)/ChangeLog; }	\
	fi
