/* Special .init and .fini section support for CRIS and CRISv32.
   Copyright (C) 2000, 2003 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   In addition to the permissions in the GNU Library General Public
   License, the Free Software Foundation gives you unlimited
   permission to link the compiled version of this file with other
   programs, and to distribute those programs without any restriction
   coming from the use of this file.  (The Library General Public
   License restrictions do apply in other respects; for example, they
   cover modification of the file, and distribution when not linked
   into another program.)

   The GNU C Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* This file is compiled into assembly code which is then munged by a sed
   script into two files: crti.s and crtn.s.

   * crti.s puts a function prologue at the beginning of the
   .init and .fini sections and defines global symbols for
   those addresses, so they can be called as functions.

   * crtn.s puts the corresponding function epilogues
   in the .init and .fini sections.  */

__asm__ ("								\n\
#include \"defs.h\"							\n\
#include \"sysdep.h\"							\n\
/*@HEADER_ENDS*/							\n\
									\n\
/*@TESTS_BEGIN*/							\n\
									\n\
/*@TESTS_END*/								\n\
									\n\
/*@_init_PROLOG_BEGINS*/						\n\
	.section .init							\n\
	.align 1							\n\
	.global	_init							\n\
	.type	_init,@function						\n\
_init:									\n\
	subq	4,$sp							\n\
	move.d	$r1,[$sp]						\n\
	move	$srp,$r1						\n\
	SETUP_PIC							\n\
	; FIXME: Call weak __gmon_start__ if != 0			\n\
        ; or arrange call_gmon_start.					\n\
	END_INIT							\n\
/*@_init_PROLOG_ENDS*/							\n\
									\n\
/*@_init_EPILOG_BEGINS*/						\n\
	.section .init							\n\
	TEARDOWN_PIC							\n\
	move	$r1,$srp						\n\
	move.d	[$sp+],$r1						\n\
	Ret								\n\
	nop								\n\
	END_INIT							\n\
/*@_init_EPILOG_ENDS*/							\n\
									\n\
/*@_fini_PROLOG_BEGINS*/						\n\
	.section .fini							\n\
	.align 1							\n\
	.global	_fini							\n\
	.type	_fini,@function						\n\
_fini:									\n\
	subq	4,$sp							\n\
	move.d	$r1,[$sp]						\n\
	move	$srp,$r1						\n\
	SETUP_PIC							\n\
	END_FINI							\n\
/*@_fini_PROLOG_ENDS*/							\n\
									\n\
/*@_fini_EPILOG_BEGINS*/						\n\
	.section .fini							\n\
	TEARDOWN_PIC							\n\
	move	$r1,$srp						\n\
	move.d	[$sp+],$r1						\n\
	Ret								\n\
	nop								\n\
	END_FINI							\n\
/*@_fini_EPILOG_ENDS*/							\n\
									\n\
/*@TRAILER_BEGINS*/							\n\
");
