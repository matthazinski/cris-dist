/* Clear given exceptions in current floating-point environment.
   Copyright (C) 1997, 1999, 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Richard Henderson <rth@tamu.edu>, 1997.

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

#include <fenv_libc.h>

int
__feclearexcept (int excepts)
{
  unsigned long int swcr;

  /* Get the current state.  */
  swcr = __ieee_get_fp_control ();

  /* Clear the relevant bits.  */
  swcr &= ~((unsigned long int) excepts & SWCR_STATUS_MASK);

  /* Put the new state in effect.  */
  __ieee_set_fp_control (swcr);

  /* Success.  */
  return 0;
}
strong_alias (__feclearexcept, __old_feclearexcept)
symbol_version (__old_feclearexcept, feclearexcept, GLIBC_2.1);
default_symbol_version (__feclearexcept, feclearexcept, GLIBC_2.2);
