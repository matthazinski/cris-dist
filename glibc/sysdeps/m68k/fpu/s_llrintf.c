/* Round argument to nearest integral value according to current rounding
   direction.
   Copyright (C) 1997, 1999 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Andreas Schwab <schwab@issan.informatik.uni-dortmund.de>

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
#include "math_private.h"
#include "mathimpl.h"

long long int
__llrintf (float x)
{
  int32_t e;
  u_int32_t i, s;
  long long int result;

  x = __m81_u(__rintf) (x);

  GET_FLOAT_WORD (i, x);

  e = ((i >> 23) & 0xff) - 0x7f;
  if (e < 0)
    return 0;
  s = i;
  i &= 0x7fffff;
  i |= 0x800000;

  if (e < 63)
    {
      if (e > 55)
	result = (long long int) (i << (e - 55)) << 32;
      else if (e > 31)
	result = (((long long int) (i >> (55 - e)) << 32) | (i << (e - 23)));
      else if (e > 23)
	result = i << (e - 23);
      else
	result = i >> (23 - e);
      if (s & 0x80000000)
	result = -result;
    }
  else
    /* The number is too large or not finite.  The standard leaves it
       undefined what to return when the number is too large to fit in a
       `long long int'.  */
    result = -1LL;

  return result;
}

weak_alias (__llrintf, llrintf)
