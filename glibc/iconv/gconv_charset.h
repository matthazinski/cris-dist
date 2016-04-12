/* Charset name normalization.
   Copyright (C) 2001 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 2001.

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

#include <ctype.h>


static inline void
strip (char *wp, const char *s)
{
  int slash_count = 0;

  while (*s != '\0')
    {
      if (isalnum (*s) || *s == '_' || *s == '-' || *s == '.')
	*wp++ = toupper (*s);
      else if (*s == '/')
	{
	  if (++slash_count == 3)
	    break;
	  *wp++ = '/';
	}
      ++s;
    }

  while (slash_count++ < 2)
    *wp++ = '/';

  *wp = '\0';
}


static char *
upstr (char *dst, const char *str)
{
  char *cp = dst;
  while ((*cp++ = toupper (*str++)) != '\0')
    /* nothing */;
  return dst;
}


/* If NAME is an codeset alias expand it.  */
extern const char *__gconv_lookup_alias (const char *name);
