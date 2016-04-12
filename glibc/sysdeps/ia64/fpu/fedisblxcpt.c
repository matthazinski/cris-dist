/* Disable floating-point exceptions.
   Copyright (C) 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Jes Sorensen <Jes.Sorensen@cern.ch>, 2000.

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
fedisableexcept (int excepts)
{
  fenv_t old_fpsr;
  fenv_t new_fpsr;

  /* Get the current fpsr.  */
  __asm__ __volatile__ ("mov.m %0=ar.fpsr" : "=r" (old_fpsr));

  new_fpsr = old_fpsr | ((fenv_t) excepts & FE_ALL_EXCEPT);

  __asm__ __volatile__ ("mov.m ar.fpsr=%0" :: "r" (new_fpsr) : "memory");

  return (old_fpsr ^ FE_ALL_EXCEPT) & FE_ALL_EXCEPT;
}
