/* Clear given exceptions in current floating-point environment.
   Copyright (C) 1997, 1999, 2000 Free Software Foundation, Inc.
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

#include <fenv.h>

int
__feclearexcept (int excepts)
{
  fexcept_t fpsr;

  /* Mask out unsupported bits/exceptions.  */
  excepts &= FE_ALL_EXCEPT;

  /* Fetch the fpu status register.  */
  __asm__ ("fmove%.l %/fpsr,%0" : "=dm" (fpsr));

  /* Clear the relevant bits.  */
  fpsr &= ~excepts;

  /* Put the new data in effect.  */
  __asm__ __volatile__ ("fmove%.l %0,%/fpsr" : : "dm" (fpsr));

  /* Success.  */
  return 0;
}
strong_alias (__feclearexcept, __old_feclearexcept)
symbol_version (__old_feclearexcept, feclearexcept, GLIBC_2.1);
default_symbol_version (__feclearexcept, feclearexcept, GLIBC_2.2);