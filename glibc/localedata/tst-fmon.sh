#! /bin/sh
# Testing the implementation of strfmon(3).
# Copyright (C) 1996, 1997, 1998, 2000 Free Software Foundation, Inc.
# This file is part of the GNU C Library.
# Contributed by Jochen Hein <jochen.hein@delphi.central.de>, 1997.
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
datafile=$3

here=`pwd`

lang=`sed -e '/^#/d' -e '/^$/d' -e '/^C	/d' -e '/^tstfmon/d' -e 's/^\([^	]*\).*/\1/' $datafile | sort | uniq`

# Generate data files.
for cns in `cd ./tst-fmon-locales && ls tstfmon_*`; do
    cn=tst-fmon-locales/$cns
    fn=charmaps/ISO-8859-1
    I18NPATH=. GCONV_PATH=${common_objpfx}iconvdata \
    LOCPATH=${common_objpfx}localedata LC_ALL=C LANGUAGE=C \
    ${run_program_prefix} ${common_objpfx}locale/localedef \
    --quiet -i $cn -f $fn ${common_objpfx}localedata/$cns
done

# Run the tests.
# There's a TAB for IFS
while IFS="	" read locale format value expect; do
    if [ -n "$format" ]; then
	LOCPATH=${common_objpfx}localedata \
	GCONV_PATH=${common_objpfx}/iconvdata \
	${run_program_prefix} ${common_objpfx}localedata/tst-fmon \
	    "$locale" "$format" "$value" "$expect"
	if [ $? -eq 0 ]; then
	    echo "Locale: \"${locale}\" Format: \"${format}\"" \
		 "Value: \"${value}\" Expect: \"${expect}\"  passed"
	else
	    echo "Locale: \"${locale}\" Format: \"${format}\"" \
		 "Value: \"${value}\" Expect: \"${expect}\"    failed"
	fi
    fi
done < $datafile

exit $?
# Local Variables:
#  mode:shell-script
# End:
