/* Change a port cell to a directory in an open file descriptor.
   Copyright (C) 1999 Free Software Foundation, Inc.
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
#include <unistd.h>
#include <hurd.h>
#include <hurd/port.h>
#include <hurd/fd.h>
#include <fcntl.h>

int
_hurd_change_directory_port_from_fd (struct hurd_port *portcell, int fd)
{
  int ret;
  struct hurd_fd *d = _hurd_fd_get (fd);

  if (!d)
    return __hurd_fail (EBADF);

  HURD_CRITICAL_BEGIN;

  ret = HURD_PORT_USE (&d->port,
		       ({
			 int ret;
			 file_t dir = __file_name_lookup_under (port, ".",
								0, 0);
			 if (dir == MACH_PORT_NULL)
			   ret = -1;
			 else
			   {
			     _hurd_port_set (portcell, dir);
			     ret = 0;
			   }
			 ret;
		       }));

  HURD_CRITICAL_END;

  return ret;
}
