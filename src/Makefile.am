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

scheme_sources = scheme/base.scm scheme/base.sld			\
                 scheme/case-lambda.sld scheme/char.sld			\
                 scheme/complex.sld scheme/cxr.sld scheme/eval.sld	\
                 scheme/file.sld scheme/inexact.sld scheme/lazy.sld	\
                 scheme/load.scm scheme/load.sld			\
                 scheme/process-context.sld scheme/r5rs.scm		\
                 scheme/r5rs.sld scheme/read.sld scheme/repl.scm	\
                 scheme/repl.sld scheme/write.sld srfi/1.scm		\
                 srfi/1.sld srfi/2.scm srfi/2.sld srfi/8.scm		\
                 srfi/8.sld srfi/28.scm srfi/28.sld srfi/37.scm		\
                 srfi/37.sld srfi/59.scm srfi/59.sld srfi/64.scm	\
                 srfi/64.sld srfi/111.scm srfi/111.sld srfi/125.sld	\
                 srfi/128.scm srfi/128.sld srfi/139.sld srfi/158.scm	\
                 srfi/158.sld srfi/188.sld srfi/190.scm srfi/190.sld	\
                 srfi/211/identifier-syntax.scm				\
                 srfi/211/identifier-syntax.sld				\
                 srfi/211/syntax-case.scm srfi/211/syntax-case.sld	\
                 srfi/211/syntax-parameter.sld				\
                 srfi/211/variable-transformer.sld			\
                 srfi/211/with-ellipsis.sld				\
                 unsyntax/auxiliary-syntax.scm				\
                 unsyntax/auxiliary-syntax.sld unsyntax/backend.scm	\
                 unsyntax/backend.sld unsyntax/bootstrap.scm		\
                 unsyntax/bootstrap.sld unsyntax/builder.scm		\
                 unsyntax/builder.sld unsyntax/command-line.scm		\
                 unsyntax/command-line.sld				\
                 unsyntax/core-procedures.sld unsyntax/define.scm	\
                 unsyntax/define.sld unsyntax/derived-forms.scm		\
                 unsyntax/derived-forms.sld				\
                 unsyntax/define-record-type.scm			\
                 unsyntax/define-record-type.sld			\
                 unsyntax/environment.scm unsyntax/environment.sld	\
                 unsyntax/error.scm unsyntax/error.sld			\
                 unsyntax/eval.sld unsyntax/expander.sld		\
                 unsyntax/expander/auxiliary-syntax.scm			\
                 unsyntax/expander/cond-expand.scm			\
                 unsyntax/expander/core-bindings.scm			\
                 unsyntax/expander/core-forms.scm			\
                 unsyntax/expander/core-syntax.scm			\
                 unsyntax/expander/core-transformers.scm		\
                 unsyntax/expander/eval.scm				\
                 unsyntax/expander/expand.scm				\
                 unsyntax/expander/macro.scm				\
                 unsyntax/expander/library-manager.scm			\
                 unsyntax/expander/primitives.scm			\
                 unsyntax/expander/syntax-case.scm			\
                 unsyntax/features.scm unsyntax/features.sld		\
                 unsyntax/gensym.scm unsyntax/gensym.sld		\
                 unsyntax/identifier.scm unsyntax/identifier.sld	\
                 unsyntax/lambda.scm unsyntax/lambda.sld		\
                 unsyntax/let.scm unsyntax/let.sld			\
                 unsyntax/library.sld unsyntax/library.scm		\
                 unsyntax/library-locator.scm				\
                 unsyntax/library-locator.sld unsyntax/position.scm	\
                 unsyntax/position.sld unsyntax/program.scm		\
                 unsyntax/program.sld unsyntax/program-name.scm		\
                 unsyntax/program-name.sld unsyntax/read-syntax.scm	\
                 unsyntax/read-syntax.sld unsyntax/runtime.sld		\
                 unsyntax/scheme.scm unsyntax/scheme.sld		\
                 unsyntax/source-location.scm				\
                 unsyntax/source-location.sld				\
                 unsyntax/source-port.scm unsyntax/source-port.sld	\
                 unsyntax/store.sld unsyntax/store/context.scm		\
                 unsyntax/store/library-table.scm			\
                 unsyntax/store/store.scm unsyntax/syntax.scm		\
                 unsyntax/syntax.sld unsyntax/syntax-case.scm		\
                 unsyntax/syntax-case.sld unsyntax/syntax-object.scm	\
                 unsyntax/syntax-object.sld unsyntax/unicode.scm	\
                 unsyntax/unicode.sld unsyntax/variable.scm		\
                 unsyntax/variable.sld					\
                 unsyntax/variable-transformer.scm			\
                 unsyntax/variable-transformer.sld			\
                 unsyntax/version-etc.scm unsyntax/version-etc.sld

nobase_dist_pkgdata_DATA = unsyntax/define-record-type.scm		\
                           unsyntax/define-record-type.sld		\
                           unsyntax/gensym.scm unsyntax/gensym.sld	\
                           unsyntax/stdlibs/runtime.scm			\
                           unsyntax/stdlibs/runtime-exports.scm		\
                           unsyntax/syntax-object.scm			\
                           unsyntax/syntax-object.sld
nobase_pkgdata_DATA = unsyntax/stdlibs.sld
EXTRA_DIST = build-stdlibs.scm stdlibs.scm unsyntax/config.sld.in	\
             $(scheme_sources)
CLEANFILES = unsyntax/config.sld unsyntax/stdlibs.sld

do_subst = $(SED) -e 's,[@]PACKAGE_NAME[@],$(PACKAGE_NAME),g' \
            -e 's,[@]PACKAGE_BUGREPORT[@],$(PACKAGE_BUGREPORT),g' \
            -e 's,[@]PACKAGE_URL[@],$(PACKAGE_URL),g' \
            -e 's,[@]PACKAGE_VERSION[@],$(PACKAGE_VERSION),g' \
            -e 's,[@]BINDIR[@],$(bindir),g' \
            -e 's,[@]PKGDATADIR[@],$(pkgdatadir),g'

scheme = env CHIBI_MODULE_PATH= $(chibi_scheme) -A$(builddir)	\
         -A$(srcdir) -h4G $(SCHEME_FLAGS)

unsyntax/config.sld: unsyntax/config.sld.in Makefile
	$(AM_V_at) $(MKDIR_P) unsyntax
	$(AM_V_GEN) $(do_subst) < $(srcdir)/unsyntax/config.sld.in	\
	> unsyntax/config.sld

unsyntax/stdlibs.sld: build-stdlibs.scm stdlibs.scm unsyntax/config.sld $(scheme_sources)
	$(AM_V_GEN) $(scheme) $(srcdir)/build-stdlibs.scm -I$(builddir) \
	-I$(srcdir) -o unsyntax/stdlibs.sld $(srcdir)/stdlibs.scm
