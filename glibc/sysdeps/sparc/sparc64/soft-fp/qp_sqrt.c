/* Software floating-point emulation.
   (*c) = sqrtl(*a)
   Copyright (C) 1997,1999 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Richard Henderson (rth@cygnus.com) and
		  Jakub Jelinek (jj@ultra.linux.cz).

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If
   not, write to the Free Software Foundation, Inc.,
   59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "soft-fp.h"
#include "quad.h"

void _Qp_sqrt(long double *c, const long double *a)
{
  FP_DECL_EX;
  FP_DECL_Q(A); FP_DECL_Q(C);

  FP_INIT_ROUNDMODE;
  FP_UNPACK_QP(A, a);
  FP_SQRT_Q(C, A);
  FP_PACK_QP(c, C);
  QP_HANDLE_EXCEPTIONS(__asm (
	"ldd [%1], %%f52
	 ldd [%1+8], %%f54
	 fsqrtq %%f52, %%f60
	 std %%f60, [%0]
	 std %%f62, [%0+8]
	" : : "r" (c), "r" (a) : QP_CLOBBER));
}
