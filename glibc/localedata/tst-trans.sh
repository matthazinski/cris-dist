#! /bin/sh
# Test character mapping definitions.
# Copyright (C) 1999, 2000 Free Software Foundation, Inc.
# This file is part of the GNU C Library.
#
# The GNU C Library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# The GNU C Library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public
# License along with the GNU C Library; see the file COPYING.LIB.  If
# not, write to the Free Software Foundation, Inc.,
# 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

common_objpfx=$1
run_program_prefix=$2

# Generate the necessary locale data.
I18NPATH=. GCONV_PATH=${common_objpfx}/iconvdata \
${run_program_prefix} \
${common_objpfx}locale/localedef --quiet \
-i tests/trans.def -f charmaps/ISO-8859-1 \
${common_objpfx}localedata/tt_TT ||
exit 1

# Run the test program.
LOCPATH=${common_objpfx}localedata GCONV_PATH=${common_objpfx}iconvdata \
LC_ALL=tt_TT ${run_program_prefix} \
${common_objpfx}localedata/tst-trans > ${common_objpfx}localedata/tst-trans.out

exit $?
