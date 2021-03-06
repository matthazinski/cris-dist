/* Set a block of memory to some byte value.  For IBM S390
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.
   Contributed by Martin Schwidefsky (schwidefsky@de.ibm.com).

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

/*
 * R2 = address to memory area
 * R3 = byte to fill memory with
 * R4 = number of bytes to fill
 */

#include "sysdep.h"
#include "asm-syntax.h"

	.text
ENTRY(memset)
	ltr     %r4,%r4
	jz      .L1
	lr      %r0,%r2             # save source address
	lr      %r1,%r3             # move pad byte to R1
	lr      %r3,%r4
	sr      %r4,%r4             # no source for MVCLE, only a pad byte
	sr      %r5,%r5
.L0:    mvcle   %r2,%r4,0(%r1)      # thats it, MVCLE is your friend
	jo      .L0
	lr      %r2,%r0             # return value is source address
.L1:
	br      %r14
END(memset)
