/* Copyright (C) 1993,95,96,97,98,2000 Free Software Foundation, Inc.
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
#include "stdio.h"

#include <shlib-compat.h>

FILE*
freopen (filename, mode, fp)
     const char* filename;
     const char* mode;
     FILE* fp;
{
  FILE *result;
  CHECK_FILE (fp, NULL);
  if (!(fp->_flags & _IO_IS_FILEBUF))
    return NULL;
  _IO_cleanup_region_start ((void (*) __P ((void *))) _IO_funlockfile, fp);
  _IO_flockfile (fp);
#if SHLIB_COMPAT (libc, GLIBC_2_0, GLIBC_2_1)
  if (&_IO_stdin_used == NULL)
    /* If the shared C library is used by the application binary which
       was linked against the older version of libio, we just use the
       older one even for internal use to avoid trouble since a pointer
       to the old libio may be passed into shared C library and wind
       up here. */
    result = _IO_old_freopen (filename, mode, fp);
  else
#endif
    result = _IO_freopen (filename, mode, fp);
  if (result != NULL)
    /* unbound stream orientation */
    result->_mode = 0;
  _IO_funlockfile (fp);
  _IO_cleanup_region_end (0);
  return result;
}
