/* Copy memory block and return pointer to beginning of destination block
   For Intel 80x86, x>=6.
   This file is part of the GNU C Library.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1999.

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

#include <sysdep.h>
#include "asm-syntax.h"
#include "bp-sym.h"
#include "bp-asm.h"

#define PARMS	LINKAGE		/* no space for saved regs */
#define RTN	PARMS
#define DEST	RTN+RTN_SIZE
#define SRC	DEST+PTR_SIZE
#define LEN	SRC+PTR_SIZE

	.text
ENTRY (BP_SYM (memcpy))
	ENTER

	movl	LEN(%esp), %ecx
	movl	%edi, %eax
	movl	DEST(%esp), %edi
	movl	%esi, %edx
	movl	SRC(%esp), %esi
	CHECK_BOUNDS_BOTH_WIDE (%edi, DEST(%esp), %ecx)
	CHECK_BOUNDS_BOTH_WIDE (%esi, SRC(%esp), %ecx)

	cld
	shrl	$1, %ecx
	jnc	1f
	movsb
1:	shrl	$1, %ecx
	jnc	2f
	movsw
2:	rep
	movsl
	movl	%eax, %edi
	movl	%edx, %esi
	movl	DEST(%esp), %eax
	RETURN_BOUNDED_POINTER (DEST(%esp))

	LEAVE
	RET_PTR
END (BP_SYM (memcpy))
