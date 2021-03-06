/*
 * crt0.S -- startup file for MIPS.
 *
 * Copyright (c) 1995, 1996, 1997 Cygnus Support
 *
 * The authors hereby grant permission to use, copy, modify, distribute,
 * and license this software and its documentation for any purpose, provided
 * that existing copyright notices are retained in all copies and that this
 * notice is included verbatim in any distributions. No written agreement,
 * license, or royalty fee is required for any of the authorized uses.
 * Modifications to this software may be copyrighted by their authors
 * and need not follow the licensing terms described here, provided that
 * the new terms are clearly indicated on the first page of each file where
 * they apply.
 */

#ifdef __mips16
/* This file contains 32 bit assembly code.  */
	.set nomips16
#endif

#include "regs.S"

/*
 * Set up some room for a stack. We just grab a chunk of memory.
 */
#define STACK_SIZE  0x4000
#define GLOBAL_SIZE 0x2000

#define STARTUP_STACK_SIZE	0x0100

/* This is for referencing addresses that are not in the .sdata or
   .sbss section under embedded-pic, or before we've set up gp.  */
#ifdef __mips_embedded_pic
# ifdef __mips64
#  define LA(t,x) la t,x-PICBASE ; daddu t,s0,t
# else
#  define LA(t,x) la t,x-PICBASE ; addu t,s0,t
# endif
#else /* __mips_embedded_pic */ 
# define LA(t,x) la t,x
#endif /* __mips_embedded_pic */ 

	.comm	__memsize, 12
	.comm	__lstack, STARTUP_STACK_SIZE
	.comm   __stackbase,4

	.text
	.align	2

/* Without the following nop, GDB thinks _start is a data variable.
 * This is probably a bug in GDB in handling a symbol that is at the
 * start of the .text section.
 */
	nop

	.globl	_start
	.ent	_start
_start:
	.set	noreorder
#ifdef __mips_embedded_pic
	PICBASE = .+8
        bal	PICBASE
	nop
	move	s0,$31
#endif
#if __mips < 3
#define STATUS_MASK (SR_CU1|SR_PE)
#else
# For mips3 or mips4, turn on 64-bit addressing and additional float regs
#define STATUS_MASK (SR_CU1|SR_PE|SR_FR|SR_KX|SR_SX|SR_UX)
#endif
	li	v0, STATUS_MASK
	mtc0	v0, C0_SR
	mtc0	zero, C0_CAUSE
	nop

/* Check for FPU presence.  Don't check if we know that soft_float is
   being used.  (This also avoids illegal instruction exceptions.)  */

#ifndef __mips_soft_float
	li	t2,0xAAAA5555
	mtc1	t2,fp0		/* write to FPR 0 */
	mtc1	zero,fp1	/* write to FPR 1 */
	mfc1	t0,fp0
	mfc1	t1,fp1
	nop
	bne	t0,t2,1f	/* check for match */
	nop
	bne	t1,zero,1f	/* double check */
	nop
	j	2f		/* FPU is present. */
	nop
#endif
1:
	/* FPU is not present.  Set status register to say that. */
	li	v0, (STATUS_MASK-(STATUS_MASK & SR_CU1))
	mtc0	v0, C0_SR
	nop
2:


/* Fix high bits, if any, of the PC so that exception handling 
   doesn't get confused.  */
	LA (v0, 3f)
	jr	v0
	nop
3:
	LA (gp, _gp)				# set the global data pointer
	.end _start

/*
 * zero out the bss section.
 */
	.globl	__memsize
	.globl	get_mem_info .text
	.globl	__stack
	.globl	__global
	.globl	zerobss
	.ent	zerobss
zerobss:
	LA (v0, _fbss)
	LA (v1, _end)
3:
	sw	zero,0(v0)
	bltu	v0,v1,3b
	addiu	v0,v0,4				# executed in delay slot

	la	t0, __lstack			# make a small stack so we
	addiu	sp, t0, STARTUP_STACK_SIZE	# can run some C code
	la	a0, __memsize			# get the usable memory size
	jal	get_mem_info
	nop

	/* setup the stack pointer */
	LA (t0, __stack)			# is __stack set ?
	bne	t0,zero,4f
	nop

	/* NOTE: a0[0] contains the amount of memory available, and
	         not the last memory address. */
	lw	t0,0(a0)			# last address of memory available
	la	t1,K0BASE			# cached kernel memory
	addu	t0,t0,t1			# get the end of memory address
	/* We must subtract 24 bytes for the 3 8 byte arguments to main, in
	   case main wants to write them back to the stack.  The caller is
	   supposed to allocate stack space for parameters in registers in
	   the old MIPS ABIs.  We must do this even though we aren't passing
	   arguments, because main might be declared to have them. 

	   Some ports need a larger alignment for the stack, so we subtract
	   32, which satisifes the stack for the arguments and keeps the
	   stack pointer better aligned.  */
	subu	t0,t0,32			# and generate a starting stack-pointer
4:
	move	sp,t0				# set stack pointer
	sw	sp,__stackbase			# keep this for future ref
	.end	zerobss

/*
 * initialize target specific stuff. Only execute these
 * functions it they exist.
 */
	.globl	hardware_init_hook .text
	.globl	software_init_hook .text
	.globl	__do_global_dtors .text
	.globl	atexit .text
	.globl	exit .text
	.globl	init
	.ent	init
init:
	LA (t9, hardware_init_hook)		# init the hardware if needed
	beq	t9,zero,6f
	nop
	jal	t9
	nop
6:
	LA (t9, software_init_hook)		# init the hardware if needed
	beq	t9,zero,7f
	nop
	jal	t9
	nop
7:
	LA (a0, __do_global_dtors)
	jal	atexit
	nop

#ifdef GCRT0
	.globl	_ftext
	.globl	_extext
	LA (a0, _ftext)
	LA (a1, _etext)
	jal	monstartup
	nop
#endif

	move	a0,zero				# set argc to 0
	jal	main				# call the program start function
	nop

	# fall through to the "exit" routine
	jal	exit				# call libc exit to run the G++
						# destructors
	move	a0,v0				# pass through the exit code
	.end	init
	
/*
 * _exit -- Exit from the application. Normally we cause a user trap
 *          to return to the ROM monitor for another run. NOTE: This is
 *	    the only other routine we provide in the crt0.o object, since
 *          it may be tied to the "_start" routine. It also allows
 *          executables that contain a complete world to be linked with
 *          just the crt0.o object.
 */
	.globl	_exit
	.ent _exit
_exit:
7:
#ifdef GCRT0
	jal	_mcleanup
	nop
#endif
	# break instruction can cope with 0xfffff, but GAS limits the range:
	break	1023
	nop
	b	7b				# but loop back just in-case
	nop
	.end _exit

/* EOF crt0.S */
