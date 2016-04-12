Summary: Compiler tools for the CRIS CPU in Axis ETRAX chips.
# $Id: cris-dist.spec,v 1.39 2007/03/06 13:03:14 hp Exp $

%define name cris-dist
%define serial 64
%define version 1.%{serial}

Vendor: Axis Communications AB.
License: GNU General Public License, libraries with GNU Library General Public License
Name: %{name}
Version: %{version}
Release: 1

# The "Serial:" tag is invalid in current RPM releases, so
# we must not use it.

# These paths are what corresponds to the defaults in
# install-cris-tools (with _prefix=/usr), so we want them to be
# the default, to minimize differences to installing from the
# .tar.gz distribution.  Note that this package is not
# relocateable.  It does have the prefix and individual
# directories compile-time configurable though, so all you need
# is the source RPM or the original tarballs and an extra hour
# or two.
%define installinfo_ %(which install-info)
%define prefixdir_ %{_prefix}/local/cris
%define mandir_ %{_prefix}/local/man/man1
%define infodir_ %{_prefix}/local/info
%define pkg0 %{name}-%{version}
%define pkg1 %{name}-glibc-%{version}
%define pkg2 %{name}-linux-headers-%{version}
%define pkg3 %{name}-linux-headersv32-%{version}
%define make_ make
%define makeinfo_ makeinfo

Group: Development/Tools
Source: ftp://ftp.axis.se/pub/axis/tools/cris/compiler-kit/%{pkg0}.tar.gz

# If you find there is problems building glibc, and you don't
# need glibc installed, try any of killing the "glibc" or "linux-headers"
# directories and the installation script should cope.
Source1: ftp://ftp.axis.se/pub/axis/tools/cris/compiler-kit/%{pkg1}.tar.gz
Source2: ftp://ftp.axis.se/pub/axis/tools/cris/compiler-kit/%{pkg2}.tar.gz
Source3: ftp://ftp.axis.se/pub/axis/tools/cris/compiler-kit/%{pkg3}.tar.gz

URL: http://developer.axis.com/
Packager: Compiler contact <compiler-contact@axis.com>

# Unfortunately, the BuildRoot is only used for info and manpages.
BuildRoot: %{_tmppath}/%{name}-%{version}-buildroot
Requires: perl >= 5.004
Requires: /bin/sh /usr/bin/perl
Requires: %{installinfo_}
Provides: gcc-cris
Provides: libbfd-cris

# FIXME: Can we use BuildRequires (sp?) and stay compatible with old rpm?
#Requires: make >= 3.79
# Ditto makeinfo.
#Requires: makeinfo >= 4.1

%description
This toolsuite contains ports of GCC (gcc-cris release R%{serial} based on
gcc-3.2.1), binutils (2.12.1), newlib (post 1.8.2) and glibc (April 2001) for
the CRIS CPU core in Axis Communications ETRAX chips.  It is used to build all
ETRAX-targeted software.  For details, see
%{_defaultdocdir}/%{pkg0}/README and %{_defaultdocdir}/%{pkg0}/NEWS
(file paths may vary).

# Later versions automatically build separate packages with
# debug info.  Stop that!
%undefine _enable_debug_packages

# Redefine strip to ignore CRIS files.
%define rpmstrip %{_builddir}/%{pkg0}/rpmstrip
%define __strip %{rpmstrip}

# Similarly redefine the tools for dependency discovery; RPM
# confuses the information in the cross-compiled DSO:s with that
# of DSO:s for the host system.  It's necessary to turn off the
# internal dependency generator to make the macros have any
# effect.
%define rpmdepreq %{_builddir}/%{pkg0}/rpmdepreq
%define __find_provides %{rpmdepreq}
%define __find_requires %{rpmdepreq}
%define _use_internal_dependency_generator 0

%prep
# Test version of "make".  We re-use scraps from glibc.  Remember to
# update or sync for "GNU make 4.?" and "GNU makeinfo [5-9]".
# See "Requires:" section above for why we don't just make this a
# "requires".
echo 'Checking version of "make":'
ac_prog_version=`%{make_} --version 2>&1 | sed -n 's/^.*GNU Make[^0-9]*\([0-9][0-9.]*\).*$/\1/p'`
case $ac_prog_version in
  '') ac_prog_version="v. ?.??, bad"; ac_verc_fail=yes;;
  3.79* | 3.[89]*)
     ac_prog_version="$ac_prog_version, ok"; ac_verc_fail=no;;
  *) ac_prog_version="$ac_prog_version, bad"; ac_verc_fail=yes;;
esac
echo Version of make is "$ac_prog_version"
if test $ac_verc_fail = yes
then
  echo '*** That "make" is unusable for building this source package.'
  echo '*** Upgrade to GNU make version 3.79 or newer.'
  exit 1
fi
echo 'Checking version of "makeinfo":'
ac_prog_version=`%{makeinfo_} --version 2>&1 | sed -n 's/^.*GNU texinfo.* \([0-9][0-9.]*\).*$/\1/p'`
case $ac_prog_version in
  '') ac_prog_version="v. ?.??, bad"; ac_verc_fail=yes;;
  4.*)
     ac_prog_version="$ac_prog_version, ok"; ac_verc_fail=no;;
  *) ac_prog_version="$ac_prog_version, bad"; ac_verc_fail=yes;;
esac
echo Version of makeinfo is "$ac_prog_version"
if test $ac_verc_fail = yes
then
  echo '*** That "makeinfo" is unusable for building this source package.'
  echo '*** Upgrade to GNU texinfo version 4.0 or newer.'
  exit 1
fi
if test -e %{prefixdir_}
then
  echo "*** %{prefixdir_} exists.  Please remove it."
  exit 1
fi
rm -rf $RPM_BUILD_ROOT
%setup
%setup1 -T -D -a 1
%setup2 -T -D -a 2
%setup3 -T -D -a 3


%build
echo Building is not a separate step.


# We delete "dir" so we can just list the directories and have
# below it everything neatly indexed for installation.  This
# does not work if $RPM_BUILD_ROOT is "/", but we would never
# get here if that happens.  We have to remove directories which
# are subdirectories of each other else rpm will complain.
# Note that %{infodir_} contains a leading slash.
%install
./install-cris-tools %{prefixdir_} $RPM_BUILD_ROOT%{mandir_} $RPM_BUILD_ROOT%{infodir_} $RPM_BUILD_DIR/%{pkg0}/tmpdir </dev/null
echo > $RPM_BUILD_DIR/%{pkg0}/%{pkg0}-filelist
for d in %{prefixdir_} %{mandir_} %{infodir_}
do
  case $d in
    %{infodir_} | %{mandir_})
       find $RPM_BUILD_ROOT$d ! -type d -print \
        | sed -e "s@^$RPM_BUILD_ROOT@@g" >> $RPM_BUILD_DIR/%{pkg0}/%{pkg0}-filelist;;
    %{prefixdir_}/*) ;;
    %{mandir_}/*) ;;
    %{infodir_}/*) ;;
    *) echo $d >> $RPM_BUILD_DIR/%{pkg0}/%{pkg0}-filelist ;;
  esac
done

# Only allow executables to be stripped.  There are no CRIS
# executables here so we don't have to check for that.  I don't
# know of a way to to reach the previous definition of a macro
# within a redefinition.  Since strip is *usually* just a minor
# variant of objcopy, assume it's in the same directory as
# objcopy.
cat <<'EOF' > %{rpmstrip}
#! /bin/sh
file="%{__file}"
strip="%(dirname %{__objcopy})/strip"
opts=
one=no
skipped=no
unskipped=no
#echo "$0 called: $* :."
while test "$#" != 0
do
 case "$1" in
  -o)
   opts="$opts $1 $2"
   one=yes
   shift
   ;;
  -F | -I | -O | -K | -N | -R)
   opts="$opts $1 $2"
   shift
   ;;
  -*)
   opts="$opts $1"
   ;;
  *)
   x=`$file $1 2>/dev/null`
   if echo $x | grep executable >/dev/null
   then
    opts="$opts $1"
    unskipped=yes
   else
    #echo "not stripping2 $1"
    skipped=yes
   fi
   ;;
 esac
 shift
done
#echo ": $opts .$skipped.$unskipped.$one"
test $skipped$unskipped = yesno || $strip $opts
EOF
chmod a+x %{rpmstrip}

# Dummy tool for listing dependencies and provisions.  Input is
# presented on stdin, so we need to consume it or else RPM will
# error due to the SIGPIPE.  If we had access to the previous
# definitions of __find_{provides,requires}, we could do
# something useful, like skipping the CRIS DSO:s, but we'll
# settle for skipping them all and list the major dependencies
# manually.
cat <<'EOF' > %{rpmdepreq}
#! /bin/sh
cat > /dev/null
EOF
chmod a+x %{rpmdepreq}

# With the buildroot, we have to make the programs appear there so
# rpm doesn't get confused.
(cd / && tar fc - `echo %{prefixdir_} | sed -e s,^/,,`) | (cd $RPM_BUILD_ROOT && tar fx -)
rm -rf %{prefixdir_}


%clean
rm -rf $RPM_BUILD_ROOT

# Whoops! Can't use paths here.
%files -f %{pkg0}-filelist
%defattr(-,root,root)
%doc NEWS README
%docdir %{mandir_}
%docdir %{infodir_}

# Install info entries in the "directory" files.
%post
for f in %{infodir_}/*-cris.info
do
  %{installinfo_} $f %{infodir_}/dir
done

# Uninstall info entries from the "directory" files.
%preun
if [ $1 = 0 ]; then
  for f in %{infodir_}/*-cris.info
  do
    %{installinfo_} --remove $f %{infodir_}/dir
  done
fi
