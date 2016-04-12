/* Copyright (C) 1998,2000 Free Software Foundation, Inc.
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

#include <shlib-compat.h>
#if SHLIB_COMPAT (libc, GLIBC_2_0, GLIBC_2_1)

#define _IO_USE_OLD_IO_FILE
#include "libioP.h"
#include "stdio.h"
#include <errno.h>

int
__old_pclose (fp)
     FILE *fp;
{
#if 0
  /* Does not actually test that stream was created by popen(). Instead,
     it depends on the filebuf::sys_close() virtual to Do The Right Thing. */
  if (fp is not a proc_file)
    return -1;
#endif
  return _IO_old_fclose (fp);
}

compat_symbol (libc, __old_pclose, pclose, GLIBC_2_0);
#endif
