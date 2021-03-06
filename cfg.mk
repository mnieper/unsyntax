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

gnulib_dir = $(srcdir)/.gnulib

old_NEWS_hash = 06e30a3d9aa09c8cce2d7b41e126ba52

exclude_file_name_regexp--sc_prohibit_doubled_word = \
  ^(.*\.scm)$$
exclude_file_name_regexp--sc_prohibit_gnu_make_extensions = \
  ^lib/Makefile$$
exclude_file_name_regexp--sc_unmarked_diagnostics = \
  ^(.*\.(scm|sch))$$
exclude_file_name_regexp--sc_prohibit_test_minus_ao = \
  ^(.*\.in)$$
