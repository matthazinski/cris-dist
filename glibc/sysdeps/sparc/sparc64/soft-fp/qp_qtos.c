/* Software floating-point emulation.
   Return (float)(*a)
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
#include "single.h"
#include "quad.h"

float _Qp_qtos(const long double *a)
{
  FP_DECL_EX;
  FP_DECL_Q(A);
  FP_DECL_S(R);
  float r;

  FP_INIT_ROUNDMODE;
  FP_UNPACK_QP(A, a);
#if (2 * _FP_W_TYPE_SIZE) < _FP_FRACBITS_Q
  FP_CONV(S,Q,1,4,R,A);
#else
  FP_CONV(S,Q,1,2,R,A);
#endif
  FP_PACK_S(r, R);

  QP_HANDLE_EXCEPTIONS(__asm (
	"ldd [%1], %%f52
	 ldd [%1+8], %%f54
	 fqtos %%f52, %0
	" : "=&f" (r) : "r" (a) : QP_CLOBBER));

  return r;
}
