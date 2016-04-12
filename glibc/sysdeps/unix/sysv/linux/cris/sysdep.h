/* Assembler macros for CRIS.
   Copyright (C) 1999, 2001, 2003, 2004 Free Software Foundation, Inc.
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

#include <asm/unistd.h>
#include <sysdeps/cris/sysdep.h>
#include <sys/syscall.h>
#include "config.h"

#undef SYS_ify
#define SYS_ify(syscall_name)	(__NR_##syscall_name)


#ifdef __ASSEMBLER__

/* For Linux we can use the system call table in the header file
	/usr/include/asm/unistd.h
   of the kernel.  But these symbols do not follow the SYS_* syntax
   so we have to redefine the `SYS_ify' macro here.  */
#undef SYS_ify
#define SYS_ify(syscall_name)	__NR_##syscall_name

/* ELF-like local names start with `.L'.  */
#undef L
#define L(name)	.L##name

/* Linux uses a negative return value to indicate syscall errors,
   unlike most Unices, which use the condition codes' carry flag.

   Since version 2.1 the return value of a system call might be
   negative even if the call succeeded.  E.g., the `lseek' system call
   might return a large offset.  Therefore we must not anymore test
   for < 0, but test for a real error by making sure the value in %eax
   is a real error number.  Linus said he will make sure the no syscall
   returns a value in -1 .. -4095 as a valid result so we can safely
   test with -4095.  */

/* Syscall wrappers consist of
	#include <sysdep.h>
	PSEUDO (...)
	 ret
	PSEUDO_END (...)

   which expand to the following.  */

/* Linux takes system call arguments in registers:
	syscall number	R9
	arg 1		R10
	arg 2		R11
	arg 3		R12
	arg 4		R13
	arg 5		MOF
	arg 6		SRP

   The compiler calls us by the C convention:
	syscall number	in the DO_CALL macro
	arg 1		R10
	arg 2		R11
	arg 3		R12
	arg 4		R13
	arg 5		[SP]
	arg 6		[SP + 4]
   */

/* Note that we use "bhs", since we want to match
   (unsigned) -4096 .. 0xffffffff.  Using "ble" would match
   -4096 .. -2**31.  */
#define	PSEUDO(name, syscall_name, args) \
  ENTRY	(name)						@ \
  DOARGS_##args						@ \
  movu.w SYS_ify (syscall_name),$r9			@ \
  break	13						@ \
  cmps.w -4096,$r10					@ \
  bhs	0f						@ \
  nop							@ \
  UNDOARGS_return_##args

/* Ouch!  We have to remember not to use "ret" in assembly-code.
   ("Luckily", mnemonics are case-insensitive.)
   Note that we assume usage is exactly:
	PSEUDO (...)
	ret
	PSEUDO_END (...)
   so we can put all payload into PSEUDO (except for error handling).  */

#define ret

#define	PSEUDO_END(name) \
0:							@ \
  SETUP_PIC						@ \
  PLTJUMP (syscall_error)				@ \
  END (name)

#define DOARGS_0
#define DOARGS_1
#define DOARGS_2
#define DOARGS_3
#define DOARGS_4
#define DOARGS_5 \
  move	[$sp],$mof

/* To avoid allocating stack-space, we re-use the arg 5 (MOF) entry by
   storing SRP into it.  If called with too-few arguments, we will crash,
   but that will happen in the general case too.  */
#ifdef __arch_v32
#define DOARGS_6 \
  DOARGS_5						@ \
  addoq	4,$sp,$acr					@ \
  move	$srp,[$sp]					@ \
  move	[$acr],$srp
#else  /* not __arch_v32 */
#define DOARGS_6 \
  DOARGS_5						@ \
  move	$srp,[$sp]					@ \
  move	[$sp+4],$srp
#endif  /* not __arch_v32 */

#define UNDOARGS_return_0 \
  Ret							@ \
  nop

#define UNDOARGS_return_1 UNDOARGS_return_0
#define UNDOARGS_return_2 UNDOARGS_return_0
#define UNDOARGS_return_3 UNDOARGS_return_0
#define UNDOARGS_return_4 UNDOARGS_return_0
#define UNDOARGS_return_5 UNDOARGS_return_0

/* We assume the following code will be "ret" and "PSEUDO_END".  */
#ifdef __arch_v32
#define UNDOARGS_return_return_6 \
  move	[$sp],$acr					@ \
  jump	$acr						@ \
  nop
#else  /* not __arch_v32 */
#define UNDOARGS_return_return_6 \
  jump	[$sp]
#endif  /* not __arch_v32 */

#else  /* not __ASSEMBLER__ */

#undef INLINE_SYSCALL
#define INLINE_SYSCALL(name, nr, args...)	\
  ({						\
     unsigned long __sys_res;			\
     register unsigned long __res asm ("r10");	\
     LOAD_ARGS_c_##nr (args)			\
     register unsigned long __callno asm ("r9")	\
       = SYS_ify (name);			\
     asm volatile (LOAD_ARGS_asm_##nr (args)	\
		   CHECK_ARGS_asm_##nr		\
		   "break 13"			\
		   : "=r" (__res)		\
		   : ASM_ARGS_##nr (args)	\
		   : ASM_CLOBBER_##nr);		\
     __sys_res = __res;				\
						\
     if (__sys_res >= (unsigned long) -4096)	\
       {					\
	 __set_errno (- __sys_res);		\
	 __sys_res = (unsigned long) -1;	\
       }					\
     /* Cast needed for correct signedness.  */	\
     (long) __sys_res;				\
   })

#define LOAD_ARGS_c_0()
#define LOAD_ARGS_asm_0()
#define ASM_CLOBBER_0 "memory"
#define ASM_ARGS_0() "r" (__callno)
#define CHECK_ARGS_asm_0			\
	".ifnc %1,$r9\n\t"			\
	".err\n\t"				\
	".endif\n\t"

/* When assigning to an asm-declared register, make sure no function call
   can come between that and its use.  Hence the extra indirection, using
   temporary variables.  FIXME: Bug or undocumented beavior in GCC.  */
#define LOAD_ARGS_c_1(r10) \
	unsigned long __r10_ = (unsigned long) (r10); 		\
	LOAD_ARGS_c_0()						\
	register unsigned long __r10 __asm__ ("r10") = __r10_;
#define LOAD_ARGS_asm_1(r10) LOAD_ARGS_asm_0 ()
#define ASM_CLOBBER_1 ASM_CLOBBER_0
#define ASM_ARGS_1(r10) ASM_ARGS_0 (), "0" (__r10)
#define CHECK_ARGS_asm_1			\
	".ifnc %1-%0,$r9-$r10\n\t"		\
	".err\n\t"				\
	".endif\n\t"

#define LOAD_ARGS_c_2(r10, r11) \
	unsigned long __r11_ = (unsigned long) (r11); 		\
	LOAD_ARGS_c_1(r10)					\
	register unsigned long __r11 __asm__ ("r11") = __r11_;
#define LOAD_ARGS_asm_2(r10, r11) LOAD_ARGS_asm_1 (r10)
#define ASM_CLOBBER_2 ASM_CLOBBER_1
#define ASM_ARGS_2(r10, r11) ASM_ARGS_1 (r10), "r" (__r11)
#define CHECK_ARGS_asm_2			\
	".ifnc %1-%0-%3,$r9-$r10-$r11\n\t"	\
	".err\n\t"				\
	".endif\n\t"

#define LOAD_ARGS_c_3(r10, r11, r12) \
	unsigned long __r12_ = (unsigned long) (r12);		\
	LOAD_ARGS_c_2(r10, r11)					\
	register unsigned long __r12 __asm__ ("r12") = __r12_;
#define LOAD_ARGS_asm_3(r10, r11, r12) LOAD_ARGS_asm_2 (r10, r11)
#define ASM_CLOBBER_3 ASM_CLOBBER_2
#define ASM_ARGS_3(r10, r11, r12) ASM_ARGS_2 (r10, r11), "r" (__r12)
#define CHECK_ARGS_asm_3 CHECK_ARGS_asm_2	\
	".ifnc %4,$r12\n\t"			\
	".err\n\t"				\
	".endif\n\t"

#define LOAD_ARGS_c_4(r10, r11, r12, r13) \
	unsigned long __r13_ = (unsigned long) (r13);		\
	LOAD_ARGS_c_3(r10, r11, r12)				\
	register unsigned long __r13 __asm__ ("r13") = __r13_;
#define LOAD_ARGS_asm_4(r10, r11, r12, r13) LOAD_ARGS_asm_3 (r10, r11, r12)
#define ASM_CLOBBER_4 ASM_CLOBBER_3
#define ASM_ARGS_4(r10, r11, r12, r13) ASM_ARGS_3 (r10, r11, r12), "r" (__r13)
#define CHECK_ARGS_asm_4 CHECK_ARGS_asm_2		\
	".ifnc %4-%5,$r12-$r13\n\t"			\
	".err\n\t"					\
	".endif\n\t"

#define LOAD_ARGS_c_5(r10, r11, r12, r13, mof) \
	LOAD_ARGS_c_4(r10, r11, r12, r13)			\
	register unsigned long __mof				\
	__asm__ ("mof") = (unsigned long) (mof);
#define LOAD_ARGS_asm_5(r10, r11, r12, r13, mof) \
	LOAD_ARGS_asm_4 (r10, r11, r12, r13)
#define ASM_CLOBBER_5 ASM_CLOBBER_4
#define ASM_ARGS_5(r10, r11, r12, r13, mof) \
	ASM_ARGS_4 (r10, r11, r12, r13), "h" (mof)
#define CHECK_ARGS_asm_5 CHECK_ARGS_asm_2		\
	".ifnc %4-%5-%6,$r12-$r13-$mof\n\t"		\
	".err\n\t"					\
	".endif\n\t"

#define LOAD_ARGS_c_6(r10, r11, r12, r13, mof, srp)		\
	LOAD_ARGS_c_5(r10, r11, r12, r13, mof)			\
	register unsigned long __srp				\
	__asm__ ("srp") = (unsigned long) (srp);
#define LOAD_ARGS_asm_6(r10, r11, r12, r13, mof, srp)		\
	LOAD_ARGS_asm_5(r10, r11, r12, r13, mof)
#define ASM_CLOBBER_6 ASM_CLOBBER_5
#define ASM_ARGS_6(r10, r11, r12, r13, mof, srp) \
	ASM_ARGS_5 (r10, r11, r12, r13, mof), "x" (srp)
#define CHECK_ARGS_asm_6 CHECK_ARGS_asm_2		\
	".ifnc %4-%5-%6-%7,$r12-$r13-$mof-$srp\n\t"	\
	".err\n\t"					\
	".endif\n\t"

#endif	/* not __ASSEMBLER__ */
