/* Install given floating-point environment and raise exceptions.
   Copyright (C) 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by David Huggins-Daines <dhd@debian.org>, 2000

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
feupdateenv (const fenv_t *envp)
{
  unsigned int sw[2];

  /* Get the current exception status. */
  __asm__ ("fstd %%fr0,0(%1)" : "=m" (*sw) : "r" (sw));
  sw[0] &= (FE_ALL_EXCEPT << 27);

  /* Install new environment.  */
  fesetenv (envp);

  /* Raise the saved exception. */
  feraiseexcept (sw[0] >> 27);

  /* Success.  */
  return 0;
}
