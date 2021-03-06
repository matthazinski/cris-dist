/* Copyright (C) 2001 Free Software Foundation, Inc.
   Contributed by Martin Schwidefsky (schwidefsky@de.ibm.com).
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
#include <socketcall.h>

/* &%/$&!! preprocessor */
#define P(a, b) P2(a, b)
#define P2(a, b) a##b

        .text
/* The socket-oriented system calls are handled unusally in Linux.
   They are all gated through the single `socketcall' system call number.
   `socketcall' takes two arguments: the first is the subcode, specifying
   which socket function is being called; and the second is a pointer to
   the arguments to the specific function.

   The .S files for the other calls just #define socket and #include this.  */

#ifndef __socket
#ifndef NO_WEAK_ALIAS
#define __socket P(__,socket)
#else
#define __socket socket
#endif
#endif

#ifndef NARGS      /* The socket.o object is compiled directly ... */
#define NARGS 3
#endif

.globl __socket
ENTRY(__socket)

	/* Save registers and setup stack.  */
        stmg    %r6,%r15,48(%r15)       /* Save registers.  */
        lgr     %r1,%r15
        lg      %r0,8(%r15)             /* Load eos.  */
        ahi     %r15,-208               /* Buy stack space.  */
        stg     %r1,0(%r15)             /* Store back chain.  */
        stg     %r0,8(%r15)             /* Store eos.  */

        /* Reorder arguments.  */
#if (NARGS >= 6)
        mvc     200(8,%r15),368(%r15)   /* Move between parameter lists.  */
#endif
#if (NARGS >= 5)
        stg     %r6,192(%r15)           /* Store into parameter list.  */
#endif
#if (NARGS >= 4)
	stg     %r5,184(%r15)            /* Store into parameter list.  */
#endif
#if (NARGS >= 3)
	stg     %r4,176(%r15)            /* Store into parameter list.  */
#endif
#if (NARGS >= 2)
        stg     %r3,168(%r15)           /* Store into parameter list.  */
	stg     %r2,160(%r15)
#endif
        /* Load subcode for socket syscall.  */
        lghi    %r2,P(SOCKOP_,socket)
	la      %r3,160(%r15)           /* Load address of parameter list.  */

        /* Do the system call trap.  */
        svc     SYS_ify(socketcall)

        lg      %r15,0(%r15)            /* Load back chain.  */
        lmg     %r6,15,48(%r15)         /* Load registers.  */

	/* gpr2 is < 0 if there was an error.  */
        lghi    %r0,-125 
        clgr    %r2,%r0
        jnl     SYSCALL_ERROR_LABEL
 
	/* Successful; return the syscall's value.  */
	br      %r14

PSEUDO_END (__socket)

#ifndef NO_WEAK_ALIAS
weak_alias (__socket, socket)
#endif

