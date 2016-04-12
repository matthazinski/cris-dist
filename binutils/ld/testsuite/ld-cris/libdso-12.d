#source: expdyn1.s
#source: dsov32-1.s
#source: dsov32-2.s
#as: --pic --no-underscore --march=v32
#ld: --shared -m crislinux
#objdump: -s -T

# Check for common DSO contents; load of GOT register, branch to
# function PLT, undefined symbol, GOT reloc.

.*:     file format elf32-cris

DYNAMIC SYMBOL TABLE:
#...
0+35a g    DF \.text	0+12 dsofn4
0+350 g    DF \.text	0+2 expfn
0+2380 g    DO \.data	0+ expobj
#...
0+352 g    DF \.text	0+8 dsofn3
#...
0+      D  \*UND\*	0+ dsofn
#...
Contents of section \.rela\.got:
 02dc 28240000 0a0f0000 00000000           .*
Contents of section \.rela\.plt:
 02e8 20240000 0b0d0000 00000000 24240000  .*
 02f8 0b150000 00000000                    .*
Contents of section \.plt:
 0300 84e20401 7e7a3f7a 04f26ffa bf09b005  .*
 0310 00000000 00000000 00006f0d 0c000000  .*
 0320 6ffabf09 b0053f7e 00000000 bf0ed4ff  .*
 0330 ffffb005 6f0d1000 00006ffa bf09b005  .*
 0340 3f7e0c00 0000bf0e baffffff b005      .*
Contents of section \.text:
 034e b005b005 bfbee2ff ffffb005 7f0dba20  .*
 035e 00005f0d 1400bfbe b6ffffff b0050000  .*
Contents of section \.data:
 2380 00000000                             .*
Contents of section \.dynamic:
 2384 04000000 94000000 05000000 98020000  .*
 2394 06000000 38010000 0a000000 43000000  .*
 23a4 0b000000 10000000 03000000 14240000  .*
 23b4 02000000 18000000 14000000 07000000  .*
 23c4 17000000 e8020000 07000000 dc020000  .*
 23d4 08000000 0c000000 09000000 0c000000  .*
 23e4 00000000 00000000 00000000 00000000  .*
 23f4 00000000 00000000 00000000 00000000  .*
 2404 00000000 00000000 00000000 00000000  .*
Contents of section \.got:
 2414 84230000 00000000 00000000 26030000  .*
 2424 40030000 00000000                    .*
