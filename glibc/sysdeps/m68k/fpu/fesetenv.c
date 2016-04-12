/* Install given floating-point environment.
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
__fesetenv (const fenv_t *envp)
{
  fenv_t temp;

  /* Install the environment specified by ENVP.  But there are a few
     values which we do not want to come from the saved environment.
     Therefore, we get the current environment and replace the values
     we want to use from the environment specified by the parameter.  */
  __asm__ ("fmovem%.l %/fpcr/%/fpsr/%/fpiar,%0" : "=m" (*&temp));

  temp.__status_register &= ~FE_ALL_EXCEPT;
  temp.__control_register &= ~((FE_ALL_EXCEPT << 6) | FE_UPWARD);
  if (envp == FE_DFL_ENV)
    ;
  else if (envp == FE_NOMASK_ENV)
    temp.__control_register |= FE_ALL_EXCEPT << 6;
  else
    {
      temp.__control_register |= (envp->__control_register
				  & ((FE_ALL_EXCEPT << 6) | FE_UPWARD));
      temp.__status_register |= envp->__status_register & FE_ALL_EXCEPT;
    }

  __asm__ __volatile__ ("fmovem%.l %0,%/fpcr/%/fpsr/%/fpiar" : : "m" (*&temp));

  /* Success.  */
  return 0;
}
strong_alias (__fesetenv, __old_fesetenv)
symbol_version (__old_fesetenv, fesetenv, GLIBC_2.1);
default_symbol_version (__fesetenv, fesetenv, GLIBC_2.2);
