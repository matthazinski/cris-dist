/* Copyright (C) 1999, 2000 Free Software Foundation, Inc.
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

#include <sysdep.h>
#define _ERRNO_H
#include <bits/errno.h>

ENTRY(__syscall_error)
#if defined (EWOULDBLOCK_sys) && EWOULDBLOCK_sys != EAGAIN
	/* We translate the system's EWOULDBLOCK error into EAGAIN.
	   The GNU C library always defines EWOULDBLOCK==EAGAIN.
	   EWOULDBLOCK_sys is the original number.  */
	mov.l	.L1, r1
	cmp/eq	r1, r0
	bf	skip
	nop
	mov.l	.L2, r0
skip:
#endif
	/* Store it in errno... */
#ifndef SHARED
#ifndef _LIBC_REENTRANT
	mov.l	.L3, r1
	mov.l	r0, @r1
#else
	mov.l	.L3, r1
	sts.l	pr, @-r15
	jsr	@r1
	 mov.l	r0, @-r15
	mov.l	@r15+, r1
	lds.l	@r15+, pr
	mov.l	r1, @r0
#endif
#else
	mov.l	r12, @-r15
#ifndef _LIBC_REENTRANT
	mov	r0, r2
        mov.l	0f, r12
	mova	0f, r0
	add	r0, r12
	mov.l	.L3, r0
	mov.l	@(r0,r12), r1
	mov.l	r2, @r1
#else
	mov.l	r0, @-r15
	sts.l	pr, @-r15
        mov.l	0f, r12
	mova	0f, r0
	add	r0, r12
	mov.l	.L3, r1
	mova	.L3, r0
	add	r0, r1
	jsr	@r1
	 nop
	lds.l	@r15+, pr
	mov.l	@r15+, r1
	mov.l	r1, @r0
#endif
	mov.l	@r15+, r12
#endif
	/* And just kick back a -1.  */
	rts
	 mov	#-1, r0

	.align	2
#if defined (EWOULDBLOCK_sys) && EWOULDBLOCK_sys != EAGAIN
.L1:	.long	EWOULDBLOCK_sys
.L2:	.long	EAGAIN
#endif
#ifndef SHARED
#ifndef _LIBC_REENTRANT
.L3:	.long	C_SYMBOL_NAME(errno)
#else
.L3:	.long	C_SYMBOL_NAME(__errno_location)
#endif
#else
0:
	.long	_GLOBAL_OFFSET_TABLE_
#ifndef _LIBC_REENTRANT
.L3:	.long	C_SYMBOL_NAME(errno@GOT)
#else
.L3:	.long	C_SYMBOL_NAME(__errno_location@PLT)
#endif
#endif
END(__syscall_error)

weak_alias (__syscall_error, syscall_error)
