/* Copyright (C) 1991, 92, 93, 95, 96 Free Software Foundation, Inc.
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

/* Some systems misname the system call number macro for this.  */
#if !defined (SYS_execve) && defined (SYS_exece)
#define SYS_execve	SYS_exece
#endif
#if !defined (SYS_execve) && defined (SYS_exec)
#define SYS_execve	SYS_exec
#endif

SYSCALL__ (execve, 3)
	ret
PSEUDO_END(__execve)

weak_alias (__execve, execve)
