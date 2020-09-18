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

AC_PREREQ([2.69])
AC_INIT([Unsyntax],
        [m4_esyscmd([build-aux/git-version-gen .tarball-version])],
        [bug@unsyntax.org], [],
        [https://www.unsyntax.org])
AC_CONFIG_SRCDIR([src/build-stdlibs.scm])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADERS([config.h])
AC_REQUIRE_AUX_FILE([tap-driver.sh])
AM_INIT_AUTOMAKE([1.16 subdir-objects]
                 m4_bmatch(m4_defn([AC_PACKAGE_VERSION]), [[-_]],
                           [gnu], [gnits]))
AM_SILENT_RULES([yes])

# Checks for programs.
AC_PROG_CC
gl_EARLY
AC_PROG_MKDIR_P
AC_PROG_SED
AC_PATH_PROG([chibi_scheme], [chibi-scheme])

AM_CONDITIONAL([BUILD_FROM_GIT], [test -d "$srcdir/.git"])
AM_CONDITIONAL([GIT_CROSS_COMPILING],
               [test -d "$srcdir/.git" && test $cross_compiling = yes])
AS_IF([test $cross_compiling = no], [
  AM_MISSING_PROG([HELP2MAN], [help2man])
], [
  HELP2MAN=:
])

# For gnulib.
gl_INIT

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile
                 lib/Makefile
		 src/Makefile
		 bin/Makefile
		 man/Makefile
		 tests/Makefile])
AC_CONFIG_LINKS([src/unsyntax/stdlibs/runtime.scm:src/unsyntax/stdlibs/runtime.scm
                 src/unsyntax/stdlibs/runtime-exports.scm:src/unsyntax/stdlibs/runtime-exports.scm])

AC_OUTPUT
