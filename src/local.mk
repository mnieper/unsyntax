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

######################
# Standard Libraries #
######################

scheme_sources = %D%/rnrs/records.sls %D%/rnrs/records/inspection.sls	\
%D%/rnrs/records/procedural.sls %D%/rnrs/records/syntactic.sls		\
%D%/scheme/base.scm %D%/scheme/base.sld %D%/scheme/case-lambda.sld	\
%D%/scheme/char.sld %D%/scheme/complex.sld %D%/scheme/cxr.sld		\
%D%/scheme/eval.sld %D%/scheme/file.sld %D%/scheme/inexact.sld		\
%D%/scheme/lazy.sld %D%/scheme/load.scm %D%/scheme/load.sld		\
%D%/scheme/process-context.sld %D%/scheme/r5rs.scm			\
%D%/scheme/r5rs.sld %D%/scheme/read.sld %D%/scheme/repl.scm		\
%D%/scheme/repl.sld %D%/scheme/time.sld %D%/scheme/write.sld		\
%D%/srfi/1.scm %D%/srfi/1.sld %D%/srfi/2.scm %D%/srfi/2.sld		\
%D%/srfi/8.scm %D%/srfi/8.sld %D%/srfi/27.sld %D%/srfi/28.scm		\
%D%/srfi/28.sld %D%/srfi/37.scm %D%/srfi/37.sld %D%/srfi/59.scm		\
%D%/srfi/59.sld %D%/srfi/64.scm %D%/srfi/64.sld %D%/srfi/111.scm	\
%D%/srfi/111.sld %D%/srfi/125.sld %D%/srfi/128.scm %D%/srfi/128.sld	\
%D%/srfi/139.sld %D%/srfi/158.scm %D%/srfi/158.sld %D%/srfi/188.sld	\
%D%/srfi/190.scm %D%/srfi/190.sld %D%/srfi/206.sld			\
%D%/srfi/211/define-macro.scm %D%/srfi/211/define-macro.sld		\
%D%/srfi/211/explicit-renaming.scm %D%/srfi/211/explicit-renaming.sld	\
%D%/srfi/211/identifier-syntax.scm %D%/srfi/211/identifier-syntax.sld	\
%D%/srfi/211/implicit-renaming.sld %D%/srfi/211/low-level.sld		\
%D%/srfi/211/syntactic-closures.scm					\
%D%/srfi/211/syntactic-closures.sld %D%/srfi/211/syntax-case.scm	\
%D%/srfi/211/syntax-case.sld %D%/srfi/211/syntax-parameter.sld		\
%D%/srfi/211/variable-transformer.sld %D%/srfi/211/with-ellipsis.sld	\
%D%/srfi/212.sld %D%/srfi/213.scm %D%/srfi/213.sld %D%/unsyntax.sls	\
%D%/unsyntax/assert.sld %D%/unsyntax/auxiliary-syntax.scm		\
%D%/unsyntax/auxiliary-syntax.sld %D%/unsyntax/backend.scm		\
%D%/unsyntax/backend.sld %D%/unsyntax/bootstrap.scm			\
%D%/unsyntax/bootstrap.sld %D%/unsyntax/builder.scm			\
%D%/unsyntax/builder.sld %D%/unsyntax/command-line.scm			\
%D%/unsyntax/command-line.sld %D%/unsyntax/core-procedures.sld		\
%D%/unsyntax/derived-forms.scm %D%/unsyntax/derived-forms.sld		\
%D%/unsyntax/define-record-type.scm					\
%D%/unsyntax/define-record-type.sld %D%/unsyntax/error.scm		\
%D%/unsyntax/error.sld %D%/unsyntax/eval.sld %D%/unsyntax/expand.scm	\
%D%/unsyntax/expand.sld %D%/unsyntax/expander.sld			\
%D%/unsyntax/expander/auxiliary-syntax.scm				\
%D%/unsyntax/expander/cond-expand.scm					\
%D%/unsyntax/expander/core-bindings.scm					\
%D%/unsyntax/expander/core-forms.scm					\
%D%/unsyntax/expander/core-syntax.scm					\
%D%/unsyntax/expander/core-transformers.scm				\
%D%/unsyntax/expander/eval.scm %D%/unsyntax/expander/expand.scm		\
%D%/unsyntax/expander/macro.scm						\
%D%/unsyntax/expander/library-manager.scm				\
%D%/unsyntax/expander/primitives.scm					\
%D%/unsyntax/expander/syntax-case.scm %D%/unsyntax/features.scm		\
%D%/unsyntax/features.sld %D%/unsyntax/gensym.scm			\
%D%/unsyntax/gensym.sld %D%/unsyntax/identifier.scm			\
%D%/unsyntax/identifier.sld %D%/unsyntax/library.sld			\
%D%/unsyntax/library.scm %D%/unsyntax/interface.scm			\
%D%/unsyntax/interface.sld %D%/unsyntax/library-locator.scm		\
%D%/unsyntax/library-locator.sld %D%/unsyntax/position.scm		\
%D%/unsyntax/position.sld %D%/unsyntax/program.scm			\
%D%/unsyntax/program.sld %D%/unsyntax/program-name.scm			\
%D%/unsyntax/program-name.sld %D%/unsyntax/read-syntax.scm		\
%D%/unsyntax/read-syntax.sld %D%/unsyntax/rib.scm			\
%D%/unsyntax/rib.sld %D%/unsyntax/runtime.sld %D%/unsyntax/scheme.scm	\
%D%/unsyntax/scheme.sld %D%/unsyntax/source-location.scm		\
%D%/unsyntax/source-location.sld %D%/unsyntax/source-port.scm		\
%D%/unsyntax/source-port.sld %D%/unsyntax/store.sld			\
%D%/unsyntax/store/context.scm %D%/unsyntax/store/library-table.scm	\
%D%/unsyntax/store/store.scm %D%/unsyntax/symbol.scm			\
%D%/unsyntax/symbol.sld %D%/unsyntax/syntactic-closure.scm		\
%D%/unsyntax/syntactic-closure.sld %D%/unsyntax/syntax.scm		\
%D%/unsyntax/syntax.sld %D%/unsyntax/syntax-object.scm			\
%D%/unsyntax/syntax-object.sld						\
%D%/unsyntax/syntax-object/chibi-declarations.scm			\
%D%/unsyntax/transformer.scm %D%/unsyntax/transformer.sld		\
%D%/unsyntax/unicode.scm %D%/unsyntax/unicode.sld			\
%D%/unsyntax/uuid.scm %D%/unsyntax/uuid.sld %D%/unsyntax/variable.scm	\
%D%/unsyntax/variable.sld %D%/unsyntax/version-etc.scm			\
%D%/unsyntax/version-etc.sld

unsyntaxpkgdatadir = $(pkgdatadir)/unsyntax
dist_unsyntaxpkgdata_DATA = %D%/unsyntax/define-record-type.scm	\
%D%/unsyntax/define-record-type.sld %D%/unsyntax/gensym.scm	\
%D%/unsyntax/gensym.sld %D%/unsyntax/syntax-object.scm		\
%D%/unsyntax/syntax-object.sld

syntax_objectpkgdatadir = $(pkgdatadir)/unsyntax/syntax-object
dist_syntax_objectpkgdata_DATA =			\
%D%/unsyntax/syntax-object/chibi-declarations.scm


stdlibspkgdatadir = $(pkgdatadir)/unsyntax/stdlibs
dist_stdlibspkgdata_DATA = %D%/unsyntax/stdlibs/runtime.scm	\
%D%/unsyntax/stdlibs/runtime-exports.scm

unsyntaxpkgdata_DATA = %D%/unsyntax/stdlibs.sld

scheme_library = $(dist_unsyntaxpkgdata_DATA)		\
$(dist_stdlibspkgdata_DATA) $(unsyntaxpkgdata_DATA)

EXTRA_DIST += %D%/build-stdlibs.scm %D%/stdlibs.scm	\
%D%/unsyntax/config.sld.in $(scheme_sources)

CLEANFILES += %D%/unsyntax/config.sld %D%/unsyntax/stdlibs.sld

%D%/unsyntax/config.sld: %D%/unsyntax/config.sld.in Makefile
	$(AM_V_GEN)$(MKDIR_P) %D%/unsyntax
	$(AM_V_at)$(do_subst) <				\
	$(top_srcdir)/src/unsyntax/config.sld.in > $@

%D%/unsyntax/stdlibs.sld: %D%/build-stdlibs.scm %D%/stdlibs.scm	\
%D%/unsyntax/config.sld $(scheme_sources)
	$(AM_V_GEN)$(scheme) $(top_srcdir)/src/build-stdlibs.scm	\
-I$(top_builddir)/src -I$(top_srcdir)/src -o $@				\
$(top_srcdir)/src/stdlibs.scm

###########
# Scripts #
###########

bin_SCRIPTS += %D%/unsyntax-scheme %D%/expand-unsyntax %D%/compile-unsyntax
CLEANFILES += $(bin_SCRIPTS)
EXTRA_DIST += %D%/unsyntax-scheme.in %D%/expand-unsyntax.in	\
%D%/compile-unsyntax.in
AM_INSTALLCHECK_STD_OPTIONS_EXEMPT += %D%/compile-unsyntax

%D%/unsyntax-scheme: %D%/unsyntax-scheme.in Makefile $(scheme_library)
	$(AM_V_GEN)$(do_subst) < $(top_srcdir)/src/unsyntax-scheme.in > $@
	$(AM_V_at)chmod +x $@

%D%/expand-unsyntax: %D%/expand-unsyntax.in Makefile $(scheme_library)
	$(AM_V_GEN)$(do_subst) < $(top_srcdir)/src/expand-unsyntax.in > $@
	$(AM_V_at)chmod +x $@

%D%/compile-unsyntax: %D%/compile-unsyntax.in Makefile $(scheme_library)
	$(AM_V_GEN)$(do_subst) < $(top_srcdir)/src/compile-unsyntax.in > $@
	$(AM_V_at)chmod +x $@
