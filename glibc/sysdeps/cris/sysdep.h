/* Assembler macros for CRIS.
   Copyright (C) 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
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

#include <sysdeps/generic/sysdep.h>

#ifndef HAVE_ELF
#error ELF is assumed.  Generalize the code and retry.
#endif

#ifndef NO_UNDERSCORES
#error User-label prefix (underscore) assumed absent.  Generalize the code and retry.
#endif

#ifdef	__ASSEMBLER__

/* Syntactic details of assembly-code.  */

/* It is *not* generally true that "ELF uses byte-counts for .align, most
   others use log2 of count of bytes", like some neighboring configs say.
   See "align" in gas/read.c which is not overridden by
   gas/config/obj-elf.c.  It takes a log2 argument.  *Some* targets
   override it to take a byte argument.  People should read source instead
   of relying on hearsay.  */
#define ALIGNARG(log2) log2

#define ASM_TYPE_DIRECTIVE(name,typearg) .type name,typearg
#define ASM_SIZE_DIRECTIVE(name) .size name,.-name

#ifdef __arch_v32

#ifdef __PIC__

/* With PLTJUMP, you *can* get away with not setting up R0 if the jump
   is to a symbol that doesn't actually have a PLT (i.e. is non-global
   or forced local by version script).  Used in setjmp-like functions.  */
#define PLTJUMP(_x) \
  ba C_SYMBOL_NAME (_x):PLT				@ \
  nop

#define PLTCALL(_x) \
  bsr C_SYMBOL_NAME (_x):PLT				@ \
  nop

#define SETUP_PIC \
  subq 4,$sp						@ \
  move.d $r0,[$sp]					@ \
  lapc _GLOBAL_OFFSET_TABLE_,$r0

#define TEARDOWN_PIC move.d [$sp+],$r0

#else

#define PLTJUMP(_x) \
  ba C_SYMBOL_NAME (_x)				@ \
  nop

#define PLTCALL(_x) \
  bsr  C_SYMBOL_NAME (_x)				@ \
  nop

#define SETUP_PIC
#define TEARDOWN_PIC
#endif


/* If compiled for profiling, call `mcount' at the start of each function.
   FIXME: Note that profiling is not actually implemented.  This is just
   example code which might not even compile, though it is believed to be
   correct.  */
#ifdef	PROF
#define CALL_MCOUNT \
  subq 4,$sp						@ \
  move $srp,[$sp]					@ \
  subq 4,$sp						@ \
  move.d $r9,[$sp]					@ \
  subq 4,$sp						@ \
  move.d $r10,[$sp]					@ \
  subq 4,$sp						@ \
  move.d $r11,[$sp]					@ \
  subq 4,$sp						@ \
  move.d $r12,[$sp]					@ \
  subq 4,$sp						@ \
  move.d $r13,[$sp]					@ \
  SETUP_PIC						@ \
  PLTCALL (mcount)					@ \
  TEARDOWN_PIC						@ \
  move.d [$sp+],$r13					@ \
  move.d [$sp+],$r12					@ \
  move.d [$sp+],$r11					@ \
  move.d [$sp+],$r10					@ \
  move.d [$sp+],$r9					@ \
  move [$sp+],$srp
#else
#define CALL_MCOUNT		/* Do nothing.  */
#endif

#else  /* not v32 */

/* The non-PIC jump is preferred, since it does not stall, and does not
   invoke generation of a PLT.  These macros assume that R0 is set up as
   GOT register.  */

#ifdef __PIC__

/* With PLTJUMP, you *can* get away with not setting up R0 if the jump
   is to a symbol that doesn't actually have a PLT (i.e. is non-global
   or forced local by version script).  Used in setjmp-like functions.  */
#define PLTJUMP(_x) \
  add.d	C_SYMBOL_NAME (_x):PLT,$pc

#define PLTCALL(_x) \
  jsr [$r0+C_SYMBOL_NAME (_x):GOTPLT16]

#define SETUP_PIC \
  push	$r0						@ \
  move.d $pc,$r0					@ \
  sub.d	.:GOTOFF,$r0

#define TEARDOWN_PIC pop $r0
#else
#define PLTJUMP(_x) jump C_SYMBOL_NAME (_x)
#define PLTCALL(_x) jsr  C_SYMBOL_NAME (_x)
#define SETUP_PIC
#define TEARDOWN_PIC
#endif

/* If compiled for profiling, call `mcount' at the start of each function.
   FIXME: Note that profiling is not actually implemented.  This is just
   example code which might not even compile, though it is believed to be
   correct.  */
#ifdef	PROF
#define CALL_MCOUNT \
  push	$srp						@ \
  push	$r9						@ \
  push	$r10						@ \
  push	$r11						@ \
  push	$r12						@ \
  push	$r13						@ \
  SETUP_PIC						@ \
  PLTCALL (mcount)					@ \
  TEARDOWN_PIC						@ \
  pop	$r13						@ \
  pop	$r12						@ \
  pop	$r11						@ \
  pop	$r10						@ \
  pop	$r9						@ \
  pop	$srp
#else
#define CALL_MCOUNT		/* Do nothing.  */
#endif

#endif /* not v32 */

/* Define an entry point visible from C.  */
#define	ENTRY(name) \
  .text							@ \
  ASM_GLOBAL_DIRECTIVE C_SYMBOL_NAME (name) 		@ \
  ASM_TYPE_DIRECTIVE (C_SYMBOL_NAME (name), function)	@ \
  .align ALIGNARG (2) 					@ \
  C_LABEL(name)						@ \
  CALL_MCOUNT

#undef	END
#define END(name) \
  ASM_SIZE_DIRECTIVE (C_SYMBOL_NAME (name))

/* Since C identifiers are not normally prefixed with an underscore
   on this system, the asm identifier `syscall_error' intrudes on the
   C name space.  Make sure we use an innocuous name.  */
#define	syscall_error	__syscall_error
#define mcount		_mcount

#endif	/* __ASSEMBLER__ */
