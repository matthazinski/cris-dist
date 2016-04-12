/* Set floating-point environment exception handling.
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
#include <math.h>

int
__fesetexceptflag (const fexcept_t *flagp, int excepts)
{
  fexcept_t fpsr;

  /* Get the current status register.  */
  __asm__ ("fmove%.l %/fpsr,%0" : "=dm" (fpsr));

  /* Install the new exception bits in the Accrued Exception Byte.  */
  fpsr &= ~(excepts & FE_ALL_EXCEPT);
  fpsr |= *flagp & excepts & FE_ALL_EXCEPT;

  /* Store the new status register.  */
  __asm__ __volatile__ ("fmove%.l %0,%/fpsr" : : "dm" (fpsr));

  /* Success.  */
  return 0;
}
strong_alias (__fesetexceptflag, __old_fesetexceptflag)
symbol_version (__old_fesetexceptflag, fesetexceptflag, GLIBC_2.1);
default_symbol_version (__fesetexceptflag, fesetexceptflag, GLIBC_2.2);
