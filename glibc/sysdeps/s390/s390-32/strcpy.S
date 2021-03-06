/* strcpy - copy a string from source to destination. For IBM S390
   This file is part of the GNU C Library.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.
   Contributed by Martin Schwidefsky (schwidefsky@de.ibm.com).

   The GNU C Library is free software  ; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation   ; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY            ; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/*
 * R2 = address of destination
 * R3 = address of source
 */

#include "sysdep.h"
#include "asm-syntax.h"

	.text
ENTRY(strcpy)
	slr   %r0,%r0
	lr    %r1,%r2
0:      mvst  %r1,%r3
	jo    0b
	br    %r14
END(strcpy)
