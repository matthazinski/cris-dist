/* Install given floating-point environment.
   Copyright (C) 1997, 1999, 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Richard Henderson <rth@tamu.edu>, 1997

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
__fesetenv (const fenv_t *envp)
{
  unsigned long int fpcr;
  fenv_t env;

  /* Magic encoding of default values: high bit set (never possible for a
     user-space address) is not indirect.  And we don't even have to get
     rid of it since we mask things around just below.  */
  if ((long int) envp >= 0)
    env = *envp;
  else
    env = (unsigned long int) envp;

  /* Reset the rounding mode with the hardware fpcr.  Note that the following
     system call is an implied trap barrier for our modification.  */
  __asm__ __volatile__ ("excb; mf_fpcr %0" : "=f" (fpcr));
  fpcr = (fpcr & ~FPCR_ROUND_MASK) | (env & FPCR_ROUND_MASK);
  __asm__ __volatile__ ("mt_fpcr %0" : : "f" (fpcr));

  /* Reset the exception status and mask with the kernel's FP code.  */
  __ieee_set_fp_control (env & SWCR_ALL_MASK);

  /* Success.  */
  return 0;
}
strong_alias (__fesetenv, __old_fesetenv)
symbol_version (__old_fesetenv, fesetenv, GLIBC_2.1);
default_symbol_version (__fesetenv, fesetenv, GLIBC_2.2);
