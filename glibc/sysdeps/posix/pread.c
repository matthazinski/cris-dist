/* Read block from given position in file without changing file pointer.
   POSIX version.
   Copyright (C) 1997, 1998, 1999 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1997.

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

ssize_t
__libc_pread (int fd, void *buf, size_t nbyte, off_t offset)
{
  /* Since we must not change the file pointer preserve the value so that
     we can restore it later.  */
  int save_errno;
  ssize_t result;
  off_t old_offset = __libc_lseek (fd, 0, SEEK_CUR);
  if (old_offset == (off_t) -1)
    return -1;

  /* Set to wanted position.  */
  if (__libc_lseek (fd, offset, SEEK_SET) == (off_t) -1)
    return -1;

  /* Write out the data.  */
  result = __libc_read (fd, buf, nbyte);

  /* Now we have to restore the position.  If this fails we have to
     return this as an error.  But if the writing also failed we
     return this error.  */
  save_errno = errno;
  if (__libc_lseek (fd, old_offset, SEEK_SET) == (off_t) -1)
    {
      if (result == -1)
	__set_errno (save_errno);
      return -1;
    }
  __set_errno (save_errno);

  return result;
}

#ifndef __libc_pread
strong_alias (__libc_pread, __pread)
weak_alias (__libc_pread, pread)
#endif
