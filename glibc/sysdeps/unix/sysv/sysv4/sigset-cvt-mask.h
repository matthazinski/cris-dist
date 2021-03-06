/* Convert between lowlevel sigmask and libc representation of sigset_t.
   SysVr4 version.
   Copyright (C) 1998 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Joe Keane <jgk@jgk.org>.

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

#define sigset_set_old_mask(set, mask) \
  do {									      \
    unsigned long int *__ptr;						      \
    __ptr = &(set)->__sigbits[0];					      \
    __ptr[0] = (mask);							      \
    __ptr[1] = 0ul;							      \
    __ptr[2] = 0ul;							      \
    __ptr[3] = 0ul;							      \
  } while (0)

#define sigset_get_old_mask(set, mask) \
  ((mask) = (unsigned int) (set)->__sigbits[0])
