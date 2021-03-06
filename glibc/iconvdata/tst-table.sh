#!/bin/sh
# Copyright (C) 2000 Free Software Foundation, Inc.
# This file is part of the GNU C Library.
# Contributed by Bruno Haible <haible@clisp.cons.org>, 2000.
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
# License along with the GNU C Library; see the file COPYING.LIB.  If not,
# write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

# Checks that the iconv() implementation (in both directions) for a
# stateless encoding agrees with the charmap table.

common_objpfx=$1
objpfx=$2
charset=$3
charmap=$4

GCONV_PATH=${common_objpfx}iconvdata
export GCONV_PATH
LC_ALL=C
export LC_ALL

set -e

# Get the charmap.
${SHELL} tst-table-charmap.sh ${charmap:-$charset} \
  < ../localedata/charmaps/${charmap:-$charset} \
  > ${objpfx}tst-${charset}.charmap.table

# Precompute expected differences between the two iconv directions.
if test ${charset} = EUC-TW; then
  irreversible=${objpfx}tst-${charset}.irreversible
  grep '^0x8EA1' ${objpfx}tst-${charset}.charmap.table > ${irreversible}
else
  irreversible=${charset}.irreversible
fi

# iconv in one direction.
${common_objpfx}elf/ld.so --library-path $common_objpfx \
${objpfx}tst-table-from ${charset} \
  > ${objpfx}tst-${charset}.table

# iconv in the other direction.
${common_objpfx}elf/ld.so --library-path $common_objpfx \
${objpfx}tst-table-to ${charset} | sort \
  > ${objpfx}tst-${charset}.inverse.table

# Difference between the two iconv directions.
diff ${objpfx}tst-${charset}.table ${objpfx}tst-${charset}.inverse.table | \
  grep '^[<>]' | sed -e 's,^. ,,' > ${objpfx}tst-${charset}.irreversible.table

# Check 1: charmap and iconv forward should be identical.
cmp -s ${objpfx}tst-${charset}.charmap.table ${objpfx}tst-${charset}.table ||
exit 1

# Check 2: the difference between the two iconv directions.
if test -f ${irreversible}; then
  cat ${objpfx}tst-${charset}.charmap.table ${irreversible} | sort | uniq -u \
    > ${objpfx}tst-${charset}.tmp.table
  cmp -s ${objpfx}tst-${charset}.tmp.table ${objpfx}tst-${charset}.inverse.table ||
  exit 1
else
  cmp -s ${objpfx}tst-${charset}.table ${objpfx}tst-${charset}.inverse.table ||
  exit 1
fi

exit 0
