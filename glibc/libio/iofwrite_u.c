/* Copyright (C) 1993, 96, 97, 98, 99, 2000 Free Software Foundation, Inc.
   This file is part of the GNU IO Library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does
   not cause the resulting executable to be covered by the GNU General
   Public License.  This exception does not however invalidate any
   other reasons why the executable file might be covered by the GNU
   General Public License.  */

#include "libioP.h"
#include <stdio.h>

#undef fwrite_unlocked

_IO_size_t
fwrite_unlocked (buf, size, count, fp)
     const void *buf;
     _IO_size_t size;
     _IO_size_t count;
     _IO_FILE *fp;
{
  _IO_size_t request = size * count;
  _IO_size_t written = 0;
  CHECK_FILE (fp, 0);
  if (request == 0)
    return 0;
  if (_IO_fwide (fp, -1) == -1)
    {
      written = _IO_sputn (fp, (const char *) buf, request);
      if (written == request)
	return count;
    }

  return written / size;
}
