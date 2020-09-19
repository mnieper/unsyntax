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

bin_SCRIPTS = unsyntax-scheme expand-unsyntax compile-unsyntax
CLEANFILES = $(bin_SCRIPTS)
EXTRA_DIST = unsyntax-scheme.in expand-unsyntax.in compile-unsyntax.in
AM_INSTALLCHECK_STD_OPTIONS_EXEMPT = compile-unsyntax

do_subst = $(SED) -e 's,[@]PACKAGE_NAME[@],$(PACKAGE_NAME),g' \
            -e 's,[@]PACKAGE_BUGREPORT[@],$(PACKAGE_BUGREPORT),g' \
            -e 's,[@]PACKAGE_URL[@],$(PACKAGE_URL),g' \
            -e 's,[@]PACKAGE_VERSION[@],$(PACKAGE_VERSION),g' \
            -e 's,[@]PKGDATADIR[@],$(pkgdatadir),g'

unsyntax-scheme: unsyntax-scheme.in Makefile
	$(AM_V_GEN)$(do_subst) < $(srcdir)/unsyntax-scheme.in > $@
	$(AM_V_at)chmod +x $@

expand-unsyntax: expand-unsyntax.in Makefile
	$(AM_V_GEN)$(do_subst) < $(srcdir)/expand-unsyntax.in > $@
	$(AM_V_at)chmod +x $@

compile-unsyntax: compile-unsyntax.in Makefile
	$(AM_V_GEN)$(do_subst) < $(srcdir)/compile-unsyntax.in > $@
	$(AM_V_at)chmod +x $@
