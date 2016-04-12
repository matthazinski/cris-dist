/* Copyright (C) 1993, 95, 96, 97, 98, 99, 2000 Free Software Foundation, Inc.
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

void
_IO_setbuffer (fp, buf, size)
     _IO_FILE *fp;
     char *buf;
     _IO_size_t size;
{
  CHECK_FILE (fp, );
  _IO_cleanup_region_start ((void (*) (void *)) _IO_funlockfile, fp);
  _IO_flockfile (fp);
  fp->_flags &= ~_IO_LINE_BUF;
  if (!buf)
    size = 0;
  (void) _IO_SETBUF (fp, buf, size);
  if (fp->_vtable_offset == 0 && fp->_mode == 0 && _IO_CHECK_WIDE (fp))
    /* We also have to set the buffer using the wide char function.  */
    (void) _IO_WSETBUF (fp, buf, size);
  _IO_funlockfile (fp);
  _IO_cleanup_region_end (0);
}

#ifdef weak_alias
weak_alias (_IO_setbuffer, setbuffer)
#endif
