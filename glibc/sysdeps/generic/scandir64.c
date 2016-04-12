/* Copyright (C) 2000 Free Software Foundation, Inc.
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

#include <dirent.h>

#define SCANDIR scandir64
#define READDIR __readdir64
#define DIRENT_TYPE struct dirent64

int scandir64 (__const char *__restrict __dir,
	       struct dirent64 ***__restrict __namelist,
	       int (*__selector) (__const struct dirent64 *),
	       int (*__cmp) (__const void *, __const void *));

#include <dirent/scandir.c>