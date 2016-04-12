/* Copyright (C) 1991, 92, 93, 94, 95, 97 Free Software Foundation, Inc.
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
#include <sys/time.h>
#include <hurd.h>
#include <hurd/port.h>

/* Set the current time of day and timezone information.
   This call is restricted to the super-user.  */
int
__settimeofday (tv, tz)
     const struct timeval *tv;
     const struct timezone *tz;
{
  error_t err;
  mach_port_t hostpriv, devmaster;

  if (tz != NULL)
    {
      errno = ENOSYS;
      return -1;
    }

  if (err = __USEPORT (PROC, __proc_getprivports (port,
						  &hostpriv, &devmaster)))
    return __hurd_fail (err);
  __mach_port_deallocate (__mach_task_self (), devmaster);

  /* `time_value_t' and `struct timeval' are in fact identical with the
     names changed.  */
  err = __host_set_time (hostpriv, *(time_value_t *) tv);
  __mach_port_deallocate (__mach_task_self (), hostpriv);

  if (err)
    return __hurd_fail (err);

  return 0;
}

weak_alias (__settimeofday, settimeofday)
