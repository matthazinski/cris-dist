/* ix87 specific frexp implementation for long double.
   Copyright (C) 1997, 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1997.

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

#include <machine/asm.h>
#include "bp-sym.h"
#include "bp-asm.h"

#ifdef __ELF__
	.section .rodata
#else
	.text
#endif

	.align ALIGNARG(4)
	ASM_TYPE_DIRECTIVE(two64,@object)
two64:	.byte 0, 0, 0, 0, 0, 0, 0xf0, 0x43
	ASM_SIZE_DIRECTIVE(two64)
	/* The following is LDBL_MAX / ldexp (1.0, 64), the largest
	   number we can handle the normal way.  */
	ASM_TYPE_DIRECTIVE(largest,@object)
largest:
	.byte 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbe, 0x7f, 0, 0
	ASM_SIZE_DIRECTIVE(largest)

#ifdef PIC
#define MO(op) op##@GOTOFF(%edx)
#else
#define MO(op) op
#endif

#define PARMS	LINKAGE		/* no space for saved regs */
#define VAL0	PARMS
#define VAL1	VAL0+4
#define VAL2	VAL1+4
#define EXPP	VAL2+4

	.text
ENTRY (BP_SYM (__frexpl))
	ENTER

	movl	VAL0(%esp), %ecx
	movl	VAL2(%esp), %eax
	orl	VAL1(%esp), %ecx
	movl	%eax, %edx
	andl	$0x7fff, %eax
	orl	%eax, %ecx
	jz	1f
	xorl	%ecx, %ecx
	cmpl	$0x7fff, %eax
	je	1f

	cmpl	$0, %eax
	je	2f

	cmpl	$0x7fbe, %eax
	ja	4f

	fldt	VAL0(%esp)
#ifdef	PIC
	call	3f
3:	popl	%edx
	addl	$_GLOBAL_OFFSET_TABLE_+[.-3b], %edx
#endif

	fmull	MO(two64)	/* It's not necessary to use a 80bit factor */
	movl	$-64, %ecx
	fstpt	VAL0(%esp)
	fwait
	movl	VAL2(%esp), %eax
	movl	%eax, %edx
	andl	$0x7fff, %eax

2:	andl	$0x8000, %edx
	subl	$16382, %eax
	orl	$0x3ffe, %edx
	addl	%eax, %ecx
	movl	%edx, VAL2(%esp)

	/* Store %ecx in the variable pointed to by the second argument,
	   get the factor from the stack and return.  */
1:	movl	EXPP(%esp), %eax
	CHECK_BOUNDS_BOTH_WIDE (%eax, EXPP(%esp), $4)
	fldt	VAL0(%esp)
	movl	%ecx, (%eax)

	LEAVE
	ret

4:	movl	VAL2(%esp), %ecx
	movl	%ecx, %edx
	andl	$0x7fff, %ecx

	andl	$0x8000, %edx
	subl	$16382, %ecx
	orl	$0x3ffe, %edx
	movl	%edx, VAL2(%esp)
	jmp	1b
END (BP_SYM (__frexpl))
weak_alias (BP_SYM (__frexpl), BP_SYM (frexpl))
