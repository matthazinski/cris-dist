# Copyright (C) 1999, 2000, 2001, 2004 Free Software Foundation, Inc.
# This file is part of the GNU C Library.

# The GNU C Library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public License
# as published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.

# The GNU C Library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.

# You should have received a copy of the GNU Library General Public
# License along with the GNU C Library; see the file COPYING.LIB.  If not,
# write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

# We don't support long doubles as a distinct type.  We don't need to set
# this variable; it's here mostly for documentational purposes.

long-double-fcts = no

ifndef subdir
# These are only wanted for the toplevel make.

# Making install-headers does not work for the toplevel in GNU make
# 3.77 (only headers in subdirs are installed), and the construct looks
# suspicious.  Let's override it here with an explicit rule (can do
# since we can override an implicit rule).  (FIXME: That comment is now
# historical; perhaps requiring 3.79 fixed that problem.  But this one
# remains:)
#  And gnu/stubs.h really is a needed header file; it is not suitable to
# only install it with "install-others".
# Let's have a target-specific target for this.
install-headers-cris: install-headers-nosubdir $(inst_includedir)/gnu/stubs.h

# install-lib should install libc.a too.
#  If you want this to depend using a more complicated construct like in
# $(installed-libcs), go ahead and try.
# FIXME: Let's try without this, and see it it's still needed.
#install-lib: $(inst_libdir)/libc.a

endif # ! subdir

# FIXME: Note that this is a machine-specific stub; profiling is not
# implemented.
ifeq ($(subdir),gmon)
sysdep_routines += _mcount
endif

# PIC code must be assembled with special options, passed on by gcc when
# given the -fpic option.
ASFLAGS-.os = -fpic

# No string inlines from glibc, thank you.  We emit them in GCC where
# appropriate - the GCC we use is new enough that the output is sane.
CPPFLAGS += -D__NO_STRING_INLINES

# Overflow occurs at 2**15/4 (8192) symbols.  Glibc uses about 2000.
# So use -fpic: smaller-size relocs; smaller, faster code.
pic-ccflag = -fpic

# Glibc plays games in the link_warning macro with the supposedly
# formatted assembly output from gcc.  That is bad.  Nobody notices since
# gas does not assume input is formatted unless the first line says
# "#NO_APP" and no other ELF gcc port does that at present (2001-02-27),
# since the default definition of ASM_FILE_START in gcc/config/elfos.h
# does not emit #NO_APP.  (It does not matter that there are #APP/#NO_APP
# sequences elsewhere in the assembly output, such as around asm:s.)  We
# kludge around it by adding an compiler option that emits verbose
# assembly info and causes #NO_APP not to be emitted first in file (but
# rather #APP).  We do it only for files we compile, and only those that
# use link_warning.  We define $(CFLAGS)-$(@F) which is used in the
# general .c compile rule, but seems not defined by other Makefiles or
# equivalent.  Unfortunately, we have to provide a definition for every
# suffix.  FIXME: Perhaps there's a GNU make spell to make this simpler.
# Perhaps we should just supply -mpdebug in a generic CFLAGS.

CFLAGS-getwd.o = -mpdebug
CFLAGS-getwd.og = -mpdebug
CFLAGS-getwd.os = -mpdebug
CFLAGS-iogets.o = -mpdebug
CFLAGS-iogets.og = -mpdebug
CFLAGS-iogets.os = -mpdebug
CFLAGS-attr.o = -mpdebug
CFLAGS-attr.og = -mpdebug
CFLAGS-attr.os = -mpdebug
CFLAGS-mktemp.o = -mpdebug
CFLAGS-mktemp.og = -mpdebug
CFLAGS-mktemp.os = -mpdebug
CFLAGS-getpw.o = -mpdebug
CFLAGS-getpw.og = -mpdebug
CFLAGS-getpw.os = -mpdebug
CFLAGS-siggetmask.o = -mpdebug
CFLAGS-siggetmask.og = -mpdebug
CFLAGS-siggetmask.os = -mpdebug
CFLAGS-gets.o = -mpdebug
CFLAGS-gets.og = -mpdebug
CFLAGS-gets.os = -mpdebug
CFLAGS-tempnam.o = -mpdebug
CFLAGS-tempnam.og = -mpdebug
CFLAGS-tempnam.os = -mpdebug
CFLAGS-tmpnam.o = -mpdebug
CFLAGS-tmpnam.og = -mpdebug
CFLAGS-tmpnam.os = -mpdebug
CFLAGS-tmpnam_r.o = -mpdebug
CFLAGS-tmpnam_r.og = -mpdebug
CFLAGS-tmpnam_r.os = -mpdebug
CFLAGS-llseek.o = -mpdebug
CFLAGS-llseek.og = -mpdebug
CFLAGS-llseek.os = -mpdebug
CFLAGS-sigstack.o = -mpdebug
CFLAGS-sigstack.og = -mpdebug
CFLAGS-sigstack.os = -mpdebug

# These are stubs.  FIXME: In a minimal library, the ones that aren't
# referred from internal functions should be omitted.  (Any notes about
# internal use below is incomplet and might be incorect.)
CFLAGS-sigreturn.o = -mpdebug
CFLAGS-sigreturn.og = -mpdebug
CFLAGS-sigreturn.os = -mpdebug
CFLAGS-getcontext.o = -mpdebug
CFLAGS-getcontext.og = -mpdebug
CFLAGS-getcontext.os = -mpdebug
CFLAGS-setcontext.o = -mpdebug
CFLAGS-setcontext.og = -mpdebug
CFLAGS-setcontext.os = -mpdebug
CFLAGS-makecontext.o = -mpdebug
CFLAGS-makecontext.og = -mpdebug
CFLAGS-makecontext.os = -mpdebug
CFLAGS-swapcontext.o = -mpdebug
CFLAGS-swapcontext.og = -mpdebug
CFLAGS-swapcontext.os = -mpdebug
CFLAGS-fexecve.o = -mpdebug
CFLAGS-fexecve.og = -mpdebug
CFLAGS-fexecve.os = -mpdebug
CFLAGS-setlogin.o = -mpdebug
CFLAGS-setlogin.og = -mpdebug
CFLAGS-setlogin.os = -mpdebug
CFLAGS-posix_fadvise.o = -mpdebug
CFLAGS-posix_fadvise.og = -mpdebug
CFLAGS-posix_fadvise.os = -mpdebug
CFLAGS-posix_fadvise64.o = -mpdebug
CFLAGS-posix_fadvise64.og = -mpdebug
CFLAGS-posix_fadvise64.os = -mpdebug
CFLAGS-sstk.o = -mpdebug
CFLAGS-sstk.og = -mpdebug
CFLAGS-sstk.os = -mpdebug
CFLAGS-revoke.o = -mpdebug
CFLAGS-revoke.og = -mpdebug
CFLAGS-revoke.os = -mpdebug
CFLAGS-gtty.o = -mpdebug
CFLAGS-gtty.og = -mpdebug
CFLAGS-gtty.os = -mpdebug
CFLAGS-stty.o = -mpdebug
CFLAGS-stty.og = -mpdebug
CFLAGS-stty.os = -mpdebug
CFLAGS-chflags.o = -mpdebug
CFLAGS-chflags.og = -mpdebug
CFLAGS-chflags.os = -mpdebug
CFLAGS-fchflags.o = -mpdebug
CFLAGS-fchflags.og = -mpdebug
CFLAGS-fchflags.os = -mpdebug
CFLAGS-getmsg.o = -mpdebug
CFLAGS-getmsg.og = -mpdebug
CFLAGS-getmsg.os = -mpdebug
CFLAGS-putmsg.o = -mpdebug
CFLAGS-putmsg.og = -mpdebug
CFLAGS-putmsg.os = -mpdebug
CFLAGS-getpmsg.o = -mpdebug
CFLAGS-getpmsg.og = -mpdebug
CFLAGS-getpmsg.os = -mpdebug
CFLAGS-putpmsg.o = -mpdebug
CFLAGS-putpmsg.og = -mpdebug
CFLAGS-putpmsg.os = -mpdebug
CFLAGS-fattach.o = -mpdebug
CFLAGS-fattach.og = -mpdebug
CFLAGS-fattach.os = -mpdebug
CFLAGS-fdetach.o = -mpdebug
CFLAGS-fdetach.og = -mpdebug
CFLAGS-fdetach.os = -mpdebug
CFLAGS-fgetexcptflg.o = -mpdebug
CFLAGS-fgetexcptflg.og = -mpdebug
CFLAGS-fgetexcptflg.os = -mpdebug
CFLAGS-fraiseexcpt.o = -mpdebug # Referred to internally (NOP if fails.)
CFLAGS-fraiseexcpt.og = -mpdebug
CFLAGS-fraiseexcpt.os = -mpdebug
CFLAGS-fsetexcptflg.o = -mpdebug
CFLAGS-fsetexcptflg.og = -mpdebug
CFLAGS-fsetexcptflg.os = -mpdebug
CFLAGS-fclrexcpt.o = -mpdebug
CFLAGS-fclrexcpt.og = -mpdebug
CFLAGS-fclrexcpt.os = -mpdebug
CFLAGS-ftestexcept.o = -mpdebug
CFLAGS-ftestexcept.og = -mpdebug
CFLAGS-ftestexcept.os = -mpdebug
CFLAGS-fegetround.o = -mpdebug
CFLAGS-fegetround.og = -mpdebug
CFLAGS-fegetround.os = -mpdebug
CFLAGS-fegetenv.o = -mpdebug
CFLAGS-fegetenv.og = -mpdebug
CFLAGS-fegetenv.os = -mpdebug
CFLAGS-fesetround.o = -mpdebug # Referred to internally (NOP if fails.)
CFLAGS-fesetround.og = -mpdebug
CFLAGS-fesetround.os = -mpdebug
CFLAGS-fesetenv.o = -mpdebug
CFLAGS-fesetenv.og = -mpdebug
CFLAGS-fesetenv.os = -mpdebug
CFLAGS-feholdexcpt.o = -mpdebug
CFLAGS-feholdexcpt.og = -mpdebug
CFLAGS-feholdexcpt.os = -mpdebug
CFLAGS-feupdateenv.o = -mpdebug
CFLAGS-feupdateenv.og = -mpdebug
CFLAGS-feupdateenv.os = -mpdebug
CFLAGS-fegetexcept.o = -mpdebug
CFLAGS-fegetexcept.og = -mpdebug
CFLAGS-fegetexcept.os = -mpdebug
CFLAGS-fedisblxcpt.o = -mpdebug
CFLAGS-fedisblxcpt.og = -mpdebug
CFLAGS-fedisblxcpt.os = -mpdebug
CFLAGS-feenablxcpt.o = -mpdebug
CFLAGS-feenablxcpt.og = -mpdebug
CFLAGS-feenablxcpt.os = -mpdebug

# Nothing we compile here will end up in the kernel, so we don't have to
# work around the multiplication bug in CRIS v10.
sysdep-CFLAGS += -mno-mul-bug-workaround
