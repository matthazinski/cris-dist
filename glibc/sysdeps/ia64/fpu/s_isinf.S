/* Test for inf/-inf
   Copyright (C) 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Jes Sorensen <jes@linuxcare.com>, October 2000.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/*
 * isinf(x) returns 1 if x is inf, -1 for x == -inf, else 0;
 */

#include <sysdep.h>
#undef ret

ENTRY (__isinf)
{
.mfi
	fclass.m p6, p7 = farg0, @inf|@pos
	;;
}
{
.mfb
(p6)	mov ret0 = 1
(p7)	fclass.m p7, p8 = farg0, @inf|@neg
(p6)	br.ret.sptk.many rp
	;;
}
{
	.pred.rel.mutex p7,p8
(p7)	mov ret0 = -1
(p8)	mov ret0 = 0
	br.ret.sptk.many rp
}	
END (__isinf)

strong_alias (__isinf, __isinff)
strong_alias (__isinf, __isinfl)

weak_alias (__isinf, isinf)
weak_alias (__isinff, isinff)
weak_alias (__isinfl, isinfl)
