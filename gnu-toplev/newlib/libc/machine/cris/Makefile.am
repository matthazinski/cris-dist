## Process this file with automake to generate Makefile.in

AUTOMAKE_OPTIONS = cygnus

INCLUDES = $(NEWLIB_CFLAGS) $(CROSS_CFLAGS) $(TARGET_CFLAGS)

noinst_LIBRARIES = lib.a

# We also make a library with just the useful
# machine-but-not-system-specific functions, usable as an add-on together
# with e.g. glibc.
toollib_LIBRARIES = libic.a

lib_a_SOURCES = setjmp.c memcpy.c memset.c memmove.c libcdtor.c

libic_a_SOURCES = setjmp.c memcpy.c memset.c memmove.c libcdtor.c
tooldir = $(exec_prefix)/$(host_alias)
toollibdir = $(tooldir)/lib$(MULTISUBDIR)

ACLOCAL_AMFLAGS = -I ../../..
CONFIG_STATUS_DEPENDENCIES = $(newlib_basedir)/configure.host
