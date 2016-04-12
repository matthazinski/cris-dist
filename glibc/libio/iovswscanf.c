/* Copyright (C) 1993, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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
#include "strfile.h"
#include <wchar.h>

int
vswscanf (string, format, args)
     const wchar_t *string;
     const wchar_t *format;
     _IO_va_list args;
{
  int ret;
  _IO_strfile sf;
  struct _IO_wide_data wd;
#ifdef _IO_MTSAFE_IO
  _IO_lock_t lock;
  sf._sbf._f._lock = &lock;
#endif
  _IO_no_init (&sf._sbf._f, 0, 0, &wd, &_IO_wstr_jumps);
  _IO_fwide (&sf._sbf._f, 1);
  _IO_wstr_init_static (&sf._sbf._f, (wchar_t *)string, 0, NULL);
  ret = _IO_vfwscanf ((_IO_FILE *) &sf._sbf, format, args, NULL);
  return ret;
}
