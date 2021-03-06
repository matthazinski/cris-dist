/* Save current context and install the given one.
   Copyright (C) 2001 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@redhat.com>, 2001.

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

#include "ucontext_i.h"


ENTRY(__swapcontext)
	/* Load address of the context data structure we save in.  */
	movl	4(%esp), %eax

	/* Return value of swapcontext.  EAX is the only register whose
	   value is not preserved.  */
	movl	$0, oEAX(%eax)

	/* Save the 32-bit register values and the return address.  */
	movl	%ecx, oECX(%eax)
	movl	%edx, oEDX(%eax)
	movl	%edi, oEDI(%eax)
	movl	%esi, oESI(%eax)
	movl	%ebp, oEBP(%eax)
	movl	(%esp), %ecx
	movl	%ecx, oEIP(%eax)
	leal	4(%esp), %ecx
	movl	%ecx, oESP(%eax)
	movl	%ebx, oEBX(%eax)

	/* Save the FS and GS segment register.  */
	xorl	%edx, %edx
	xorl	%ecx, %ecx
	movw	%gs, %dx
	movw	%fs, %cx
	movl	%edx, oGS(%eax)
	movl	%ecx, oFS(%eax)

	/* We have separate floating-point register content memory on the
	   stack.  We use the __fpregs_mem block in the context.  Set the
	   links up correctly.  */
	leal	oFPREGSMEM(%eax), %ecx
	movl	%ecx, oFPREGS(%eax)
	/* Save the floating-point context.  */
	fnstenv	(%ecx)

	/* Load address of the context data structure we have to load.  */
	movl	8(%esp), %ecx

	/* Save the current signal mask and install the new one.  */
	pushl	%ebx
	leal	oSIGMASK(%eax), %edx
	leal	oSIGMASK(%ecx), %ecx
	movl	$SIG_SETMASK, %ebx
	movl	$__NR_sigprocmask, %eax
	int	$0x80
	popl	%ebx
	cmpl	$-4095, %eax		/* Check %eax for error.  */
	jae	SYSCALL_ERROR_LABEL	/* Jump to error handler if error.  */

	/* EAX was modified, reload it.  */
	movl	8(%esp), %eax

	/* Restore the floating-point context.  Not the registers, only the
	   rest.  */
	movl	oFPREGS(%eax), %ecx
	fldenv	(%ecx)

	/* Restore the FS and GS segment registers.  */
	movl	oGS(%eax), %edx
	movl	oFS(%eax), %ecx
	movw	%dx, %gs
	movw	%cx, %fs

	/* Load the new stack pointer.  */
	movl	oESP(%eax), %ecx
	/* Make room for 8 registers and the return address.  We will load
	   the values from the stack.  */
	subl	$36, %ecx

	/* Move the values of all the 32-bit registers (except ESP) on
	   the stack.  This happens in the form the 'popa' instruction
	   expects it.  Before this block put the address of the code
	   to execute.  */
	movl	oEDI(%eax), %ebx
	movl	oESI(%eax), %edx
	movl	oEBP(%eax), %esi
	movl	oEBX(%eax), %edi
	movl	%ebx, (%ecx)
	movl	%edx, 4(%ecx)
	movl	%esi, 8(%ecx)
	movl	%edi, 16(%ecx)
	movl	oEDX(%eax), %ebx
	movl	oECX(%eax), %edx
	movl	oEAX(%eax), %esi
	movl	oEIP(%eax), %edi
	movl	%ebx, 20(%ecx)
	movl	%edx, 24(%ecx)
	movl	%esi, 28(%ecx)
	movl	%edi, 32(%ecx)

	/* Set the new stack address.  The stack points now to the block
	   we put the register content in.  */
	movl	%ecx, %esp
	/* Restore the register content.  */
	popa
	/* The following 'ret' will pop the addres of the code and jump
	   to it.  */

L(pseudo_end):
	ret
PSEUDO_END(__swapcontext)

weak_alias(__swapcontext, swapcontext)
