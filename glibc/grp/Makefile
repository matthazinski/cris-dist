# Copyright (C) 1991, 92, 96, 97, 98, 99, 2000 Free Software Foundation, Inc.
# This file is part of the GNU C Library.

# The GNU C Library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.

# The GNU C Library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.

# You should have received a copy of the GNU Library General Public
# License along with the GNU C Library; see the file COPYING.LIB.  If not,
# write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

#
#	Sub-makefile for grp portion of the library.
#
subdir	:= grp
headers := grp.h

distribute := tst_fgetgrent.c tst_fgetgrent.sh

routines := fgetgrent initgroups setgroups \
	    getgrent getgrgid getgrnam putgrent \
	    getgrent_r getgrgid_r getgrnam_r fgetgrent_r

include ../Makeconfig

tests := testgrp

ifeq (yes,$(build-shared))
test-srcs :=  tst_fgetgrent
endif


include ../Rules

ifeq (yes,$(build-static-nss))
otherlibs += $(nssobjdir)/libnss_files.a $(resolvobjdir)/libnss_dns.a \
	     $(resolvobjdir)/libresolv.a
endif


ifeq ($(have-thread-library),yes)

CFLAGS-getgrgid_r.c = -DUSE_NSCD=1
CFLAGS-getgrnam_r.c = -DUSE_NSCD=1

endif

ifeq (no,$(cross-compiling))
# tst_fgetgrent currently only works with shared libraries
ifeq (yes,$(build-shared))
tests: $(objpfx)tst_fgetgrent.out
$(objpfx)tst_fgetgrent.out: tst_fgetgrent.sh $(objpfx)tst_fgetgrent
	$(SHELL) -e tst_fgetgrent.sh $(common-objpfx) $(elf-objpfx) \
		 $(rtld-installed-name)

endif
endif
