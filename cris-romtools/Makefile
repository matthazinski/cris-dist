#
# Makefile for CRIS romtools.
# Copyright (C) 1998, 1999, 2000, 2007 Axis Communications.
#
the_files=README ldur-cris nm-wrap strip-wrap Makefile bin-cris.sh

# In case somebody accidentally uses this
binpath=/dev/null
srcdir=/dev/null
libsubdir=/dev/null
final_libsubdir=/dev/null

.PHONY: all install clean dist distrib

all:

install:
# Disabled because cris-axis-aout is disabled at the top-level.
#	@/usr/bin/perl -e 'require 5.004; print "Hello, world";' >/dev/null \
#	 && mv $(libsubdir)/ld $(libsubdir)/rld-cris && cp -p $(srcdir)/ldur-cris $(libsubdir)/ld \
#	 && mv $(libsubdir)/nm $(libsubdir)/rnm-cris && cp -p $(srcdir)/nm-wrap $(libsubdir)/nm \
#	 && mv $(libsubdir)/strip $(libsubdir)/rstrip-cris && cp -p $(srcdir)/strip-wrap $(libsubdir)/strip \
#         || (echo '*** INSTALLATION WARNING!'; \
#             echo '*** You need to install Perl version >= 5.004 in /usr/bin for'; \
#	     echo '*** COMPILATION for ELINUX (see $(srcdir)/README) to work')
	sed -e 's,@final_libsubdir@,$(final_libsubdir),g;' < $(srcdir)/bin-cris.sh \
		> $(binpath)/bin-cris && chmod a+x $(binpath)/bin-cris
	sed -e 's,@final_libsubdir@,$(final_libsubdir),g;' < $(srcdir)/bin-cris.sh \
		> $(binpath)/cris-burn && chmod a+x $(binpath)/cris-burn

clean:
	-rm -f *~ *.o

dist distrib:
	tar fc cris-romtools.tar $(the_files)
