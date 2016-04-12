/* Copyright (C) 1999, 2000 Free Software Foundation, Inc.
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

#include <sys/socket.h>

extern int nrecvfrom (int s, void *uap_buf, int len, int flags,
		      void *uap_from, int *uap_fromlenaddr);

int
recvfrom (int fd, void *buf, size_t n, int flags, __SOCKADDR_ARG addr,
	  socklen_t *addr_len)
{
  return nrecvfrom (fd, buf, n, flags, addr.__sockaddr__, addr_len);
}
