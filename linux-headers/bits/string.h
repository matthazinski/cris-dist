/* Optimized, inlined string functions.  CRIS version.
   Copyright (C) 1997, 2001 Free Software Foundation, Inc.
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

#ifndef _STRING_H
# error "Never use <bits/string.h> directly; include <string.h> instead."
#endif

/* We can access memory on non-natural boundaries, a.k.a. unaligned
   accesses.  */
#define _STRING_ARCH_unaligned	1

/* We don't want string inlines at all unless specifically requested.  All
   current intended use is memory-constrained.  */
#ifndef __USE_STRING_INLINES
#undef __NO_STRING_INLINES
#define __NO_STRING_INLINES 1
#endif
