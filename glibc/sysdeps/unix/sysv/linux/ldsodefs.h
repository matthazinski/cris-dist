/* Run-time dynamic linker data structures for loaded ELF shared objects.
   Copyright (C) 2001 Free Software Foundation, Inc.
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

#ifndef	_LDSODEFS_H

#include "kernel-features.h"

/* Get the real definitions.  */
#include_next <ldsodefs.h>

/* Now define our stuff.  */

/* We have the auxiliary vector.  */
#define HAVE_AUX_VECTOR

/* Used by static binaries to check the auxiliary vector.  */
extern void _dl_aux_init (ElfW(auxv_t) *av) internal_function;

/* We can assume that the kernel always provides the AT_UID, AT_EUID,
   AT_GID, and AT_EGID values in the auxiliary vector.  */
#define HAVE_AUX_XID

/* Starting with one of the 2.4.0 pre-releases the Linux kernel passes
   up the page size information.  */
#if __ASSUME_AT_PAGESIZE
# define HAVE_AUX_PAGESIZE
#endif

#endif /* ldsodefs.h */