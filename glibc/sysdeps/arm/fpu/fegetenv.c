/* Store current floating-point environment.
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

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
#include <fpu_control.h>

int
__fegetenv (fenv_t *envp)
{
  unsigned long int temp;
  _FPU_GETCW (temp);
  envp->__cw = temp;

  /* Success.  */
  return 0;
}
strong_alias (__fegetenv, __old_fegetenv)
symbol_version (__old_fegetenv, fegetenv, GLIBC_2.1);
default_symbol_version (__fegetenv, fegetenv, GLIBC_2.2);
