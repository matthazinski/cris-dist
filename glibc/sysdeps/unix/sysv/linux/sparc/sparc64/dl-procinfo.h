/* Linux/sparc64 version of processor capability information handling macros.
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Jakub Jelinek <jj@ultra.linux.cz>, 1999.

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

#ifndef _DL_PROCINFO_H
#define _DL_PROCINFO_H	1

#include <ldsodefs.h>

/* If anything should be added here check whether the size of each string
   is still ok with the given array size.  */
static const char sparc64_cap_flags[][7] =
  {
    "flush", "stbar", "swap", "muldiv", "v9", "ultra3"
  };
#define _DL_HWCAP_COUNT 6

static inline int
__attribute__ ((unused))
_dl_procinfo (int word)
{
  int i;

  _dl_printf ("AT_HWCAP:   ");

  for (i = 0; i < _DL_HWCAP_COUNT; ++i)
    if (word & (1 << i))
      _dl_printf (" %s", sparc64_cap_flags[i]);

  _dl_printf ("\n");

  return 0;
}

static inline const char *
__attribute__ ((unused))
_dl_hwcap_string (int idx)
{
  return sparc64_cap_flags[idx];
};


static inline int
__attribute__ ((unused))
_dl_string_hwcap (const char *str)
{
  int i;
  for (i = 0; i < _DL_HWCAP_COUNT; i++)
    {
      if (strcmp (str, sparc64_cap_flags [i]) == 0)
	return i;
    }
  return -1;
};

#define HWCAP_IMPORTANT (HWCAP_SPARC_ULTRA3)

/* There are no different platforms defined.  */
#define _dl_platform_string(idx) ""

/* There're no platforms to filter out.  */
#define _DL_HWCAP_PLATFORM 0

#define _dl_string_platform(str) (-1)

#endif /* dl-procinfo.h */
