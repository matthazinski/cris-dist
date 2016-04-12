#! /bin/sh
###############################################################
#
# Bin-cris: get the binary stuff from a CRIS linked file
#
# Check some historic parameters, weed out outfile and infile, and get
# the binary file from the infile, write it to outfile.
#
# Sep 08 1997  H-P Nilsson   Initial.  Not in SCCS yet...
# Nov 28 2000  H-P Nilsson   See ChangeLog.
#
###############################################################
progname=$0
infile=
outfile=
width=1
block=0

trap "rm -f $tmpfiles $outfile 2>/dev/null 2>&1; exit 1" 1 2 15

usage()
{
  if test "X$1" != "X"; then
    echo "$progname: $*.  Usage:";
  fi;
  cat <<EOF
$progname [-w width] [-b block] infile [-o] outfile
Make a binary (rom-able) image from a final linked file in a.out-cris format,
with a little help from objcopy-cris.  Options:
  -w width      Number of byte-wide proms: 1=8 bits, 2=2*8 bits
  -b block      Which block to create; valid with "-w 2".  0=low, 1=high
  -o outfile    Optional; To just state outfile without "-o" works as well.
EOF
}

if test $# -eq 0
then
 usage "No parameters given"
 exit 1
fi

while test $# -ne 0
do
 case $1 in
 -w | -b | -o)
  if test $# -eq 1
  then
    usage "No parameter given to \"$1\""
    exit 1
  fi;;
 esac
   
 case $1 in
 -w)
   width=$2; shift;;
 -w*)
   width=`echo $1 | sed -e 's/-w//'`;;
 -b)
   block=$2; shift;;
 -b*)
   block=`echo $1 | sed -e 's/-b//'`;;
 -o)
   outfile=$2; shift;;
 -o*)
   outfile=`echo $1 | sed -e 's/-o//'`;;
 -h | --help | -? | -help)
   usage; exit 0;;
 -*)
   echo "Unknown parameter \"$1\"";
   usage; exit 1;;
 *)
   if test "X$infile" != "X"
   then
     outfile=$1
   else
     infile=$1
   fi;;
 esac

 shift;  
done;

tmpfile=$outfile.tmp.$$
tmpfiles=$tmpfile

if test X$width != X1 && test X$width != X2
then
 usage "Invalid width value: $width"
 exit 1;
fi

if test X$block != X0 && test X$block != X1
then
 usage "Invalid block value: $block"
 exit 1
fi

if test "X$infile" = "X"
then
 usage "No input file given"
 exit 1
fi

if test "X$outfile" = "X"
then
 usage "No output file given"
 exit 1
fi

#cat <<EOF
#-w : $width
#-b : $block
#-o : $outfile
#infile : $infile
#EOF

# We assume this script will be stored in the same directory as a
# program named objcopy-cris.  If not, try the one in PATH, and last
# check where suffix/prefixless tools were supposed to be installed.
ocexe=`dirname $0`/objcopy-cris
($ocexe --help) >/dev/null 2>/dev/null || ocexe=objcopy-cris
($ocexe --help) >/dev/null 2>/dev/null || ocexe=@final_libsubdir@/objcopy

if ($ocexe --help) >/dev/null 2>/dev/null
then
  true
else
  echo "$0: Installation problem: Cannot execute objcopy, last tried \"$ocexe\"."
  exit 2
fi

# FIXME: A bug in objcopy makes it not work correctly when using "-i"
# on objects with multiple sections.  Therefore, we dump to a
# temporary file, and use "-i" on that one.
# FIXME: A second bug/non-feature makes objcopy create sparse files
# for program sections with "gaps" in-between; i.e. when there's a gap
# between the code area and the data area.  Unfortunately, there's no
# easy way to tell objcopy to DTRT or just output the "code and
# read-only sections" when we do it in two separate runs; we have to
# find those sections and tell it which to exclude and include.  This
# only works if there are two program sections, so check for that.
# (If we're not using ELF or there's only one program section, we have
# no problem.)

reexe=`dirname $0`/readelf-cris
($reexe --help) >/dev/null 2>/dev/null || reexe=readelf-cris
($reexe --help) >/dev/null 2>/dev/null || reexe=@final_libsubdir@/readelf

if ($reexe --help) >/dev/null 2>/dev/null
then
  true
else
  echo "$0: Installation problem: Cannot execute readelf, last tried \"$reexe\"."
  exit 2
fi

# Check for two program headers.  Use portable sed constructs.
nheaders=`($reexe -l $infile) 2>/dev/null | sed -ne 's/.*There are \([0-9][0-9]*\) program headers.*/\1/p'`

ret=0

# If there's one header or if this is not an ELF file, we're safe.
if test X$nheaders != X && test X$nheaders != X1
then
  if test X$nheaders != X2
  then
    echo "$0: Cannot handle a file with more than two program headers,"
    echo "but $infile has $nheaders headers:"
    $reexe -l $infile
    exit 1
  fi

  # Get the text sections.  There's a number and spaces before the
  # first section name and after the last one.
  tsects=`$reexe -l $infile | sed -ne 's/^[ 	]*00[ 	]*\(.*\)/\1/p'`
  tsects=`echo $tsects | sed -e 's/[ 	][ 	]*$//'`

  # Copy text sections to file.
  tmpfiles="$tmpfiles $tmpfile.1"
  if $ocexe -O binary -j `echo $tsects | sed -e 's/[ 	][ 	]*/ -j /g'` $infile $tmpfile.1
  then
    # Copy other sections to file.  We assume that text sections are
    # an even number of bytes, as that is what we have the linker
    # scripts say, and that is the alignment of code sections.
    # Otherwise, we would have a song and dance inviting
    # "cat ... /dev/zero | dd ..." and friends.
    tmpfiles="$tmpfiles $tmpfile.2"
    if $ocexe -O binary -R `echo $tsects | sed -e 's/[ 	][ 	]*/ -R /g'` $infile $tmpfile.2
    then
      if test "X$width" != "X2"
      then
	cat $tmpfile.1 $tmpfile.2 > $outfile
	ret=$?
	rm -f $tmpfiles 2>/dev/null || ret=$?
	exit $ret
      else
	if cat $tmpfile.1 $tmpfile.2 > $tmpfile
	then
	  true
	else
	  ret=$?
	  rm -f $tmpfiles 2>/dev/null || ret=$?
	  exit $ret
	fi
      fi
    else
      ret=$?
    fi
  else
    ret=$?
  fi
fi

if test $ret -eq 0 && test "X$width" = "X2"
then
 if test X$nheaders = X2 || $ocexe -O binary $infile $tmpfile
 then
   # Avoid scaring user with the uninformative warning that the machine
   # type cannot be represented in a binary file.
   $ocexe -i 2 -b $block -I binary -O binary $tmpfile $outfile 2>/dev/null
 fi
 ret=$?
else
  $ocexe -O binary $infile $outfile
  ret=$?
fi
rm -f $tmpfiles 2>/dev/null || ret=$?
exit $ret
