#source: dso-2.s
#source: dsofnf.s
#source: gotrel1.s
#as: --pic --no-underscore
#ld: -shared -m crislinux
#objdump: -sR

# Make sure we merge a PLT-specific entry (usually
# R_CRIS_JUMP_SLOT) with a GOT-specific entry (R_CRIS_GLOB_DAT)
# in a DSO.  It's ok: we make a round-trip to the PLT in the
# executable if it's referenced there, but that's still
# perceived as better than having an unnecessary PLT, dynamic
# reloc and lookup in the DSO.)

.*:     file format elf32-cris

DYNAMIC RELOCATION RECORDS
OFFSET   TYPE              VALUE 
0000233c R_CRIS_GLOB_DAT   dsofn

Contents of section .*
#...
Contents of section \.rela\.got:
 027c 3c230000 0a110000 00000000           .*
Contents of section \.text:
 0288 5f1d0c00 30096f1d 0c000000 30090000  .*
 0298 6f0d0c00 0000611a 6f3e70df ffff0000  .*
Contents of section \.data:
Contents of section \.dynamic:
 22c0 04000000 94000000 05000000 48020000  .*
 22d0 06000000 28010000 0a000000 33000000  .*
 22e0 0b000000 10000000 07000000 7c020000  .*
 22f0 08000000 0c000000 09000000 0c000000  .*
 2300 00000000 00000000 00000000 00000000  .*
 2310 00000000 00000000 00000000 00000000  .*
 2320 00000000 00000000 00000000 00000000  .*
Contents of section \.got:
 2330 c0220000 00000000 00000000 00000000  .*
