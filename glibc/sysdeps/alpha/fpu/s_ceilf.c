/* Copyright (C) 1998, 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Richard Henderson.

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

#include <math.h>

/* Use the -inf rounding mode conversion instructions to implement
   ceil, via something akin to -floor(-x).  This is much faster than
   playing with the fpcr to achieve +inf rounding mode.  */

float
__ceilf (float x)
{
  if (isless (fabsf (x), 16777216.0f))	/* 1 << FLT_MANT_DIG */
    {
      /* Note that Alpha S_Floating is stored in registers in a
	 restricted T_Floating format, so we don't even need to
	 convert back to S_Floating in the end.  The initial
	 conversion to T_Floating is needed to handle denormals.  */

      float tmp1, tmp2, new_x;

      new_x = -x;
      __asm ("cvtst/s %3,%2\n\t"
#ifdef _IEEE_FP_INEXACT
	     "cvttq/svim %2,%1\n\t"
#else
	     "cvttq/svm %2,%1\n\t"
#endif
	     "cvtqt/m %1,%0\n\t"
	     : "=f"(new_x), "=&f"(tmp1), "=&f"(tmp2)
	     : "f"(new_x));

      /* Fix up the negation we did above, as well as handling -0 properly. */
      x = copysignf(new_x, x);
    }
  return x;
}

weak_alias (__ceilf, ceilf)