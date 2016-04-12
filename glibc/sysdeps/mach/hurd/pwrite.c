/* Write block at given position in file without changing file pointer.
   Hurd version.
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
#include <hurd/fd.h>

/* Write NBYTES of BUF to FD at given position OFFSET without changing
   the file position.  Return the number written, or -1.  */
ssize_t
__pwrite (int fd, const void *buf, size_t nbytes, off_t offset)
{
  error_t err = HURD_FD_USE (fd, _hurd_fd_write (descriptor,
						 buf, &nbytes, offset));
  return err ? __hurd_dfail (fd, err) : nbytes;
}
weak_alias (__pwrite, pwrite)
