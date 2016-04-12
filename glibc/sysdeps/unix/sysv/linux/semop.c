/* Copyright (C) 1995, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, August 1995.

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

#include <errno.h>
#include <sys/sem.h>
#include <ipc_priv.h>

#include <sysdep.h>
#include <sys/syscall.h>
#include <bp-checks.h>

/* Perform user-defined atomical operation of array of semaphores.  */

int
semop (semid, sops, nsops)
     int semid;
     struct sembuf *sops;
     size_t nsops;
{
  return INLINE_SYSCALL (ipc, 5, IPCOP_semop,
			 semid, (int) nsops, 0, CHECK_N (sops, nsops));
}
