/* Copyright (C) 1996, 1997, 1999, 2000 Free Software Foundation, Inc.
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

#include <errno.h>
#include <bits/libc-lock.h>
#include <stdlib.h>

#include "nsswitch.h"

/*******************************************************************\
|* Here we assume several symbols to be defined:		   *|
|*								   *|
|* LOOKUP_TYPE   - the return type of the function		   *|
|*								   *|
|* GETFUNC_NAME  - name of the non-reentrant getXXXent function	   *|
|*								   *|
|* BUFLEN	 - size of static buffer			   *|
|*								   *|
|* Optionally the following vars can be defined:		   *|
|*								   *|
|* NEED_H_ERRNO  - an extra parameter will be passed to point to   *|
|*		   the global `h_errno' variable.		   *|
|*								   *|
\*******************************************************************/

/* To make the real sources a bit prettier.  */
#define REENTRANT_GETNAME APPEND_R (GETFUNC_NAME)
#define APPEND_R(name) APPEND_R1 (name)
#define APPEND_R1(name) name##_r
#define INTERNAL(name) INTERNAL1 (name)
#define INTERNAL1(name) __##name

/* Sometimes we need to store error codes in the `h_errno' variable.  */
#ifdef NEED_H_ERRNO
# define H_ERRNO_PARM , int *h_errnop
# define H_ERRNO_VAR &h_errno
#else
# define H_ERRNO_PARM
# define H_ERRNO_VAR NULL
#endif

/* Prototype of the reentrant version.  */
extern int INTERNAL (REENTRANT_GETNAME) (LOOKUP_TYPE *resbuf, char *buffer,
					 size_t buflen, LOOKUP_TYPE **result
					 H_ERRNO_PARM);

/* We need to protect the dynamic buffer handling.  */
__libc_lock_define_initialized (static, lock);

/* This points to the static buffer used.  */
static char *buffer;


LOOKUP_TYPE *
GETFUNC_NAME (void)
{
  static size_t buffer_size;
  static LOOKUP_TYPE resbuf;
  LOOKUP_TYPE *result;
  int save;

  /* Get lock.  */
  __libc_lock_lock (lock);

  result = (LOOKUP_TYPE *)
    __nss_getent ((getent_r_function) INTERNAL (REENTRANT_GETNAME),
		  (void **) &resbuf, &buffer, BUFLEN, &buffer_size,
		  H_ERRNO_VAR);

  save = errno;
  __libc_lock_unlock (lock);
  __set_errno (save);
  return result;
}


/* Free all resources if necessary.  */
static void __attribute__ ((unused))
free_mem (void)
{
  free (buffer);
}

text_set_element (__libc_subfreeres, free_mem);
