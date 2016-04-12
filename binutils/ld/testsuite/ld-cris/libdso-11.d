#source: dso-1.s
#source: dsov32-1.s
#as: --pic --no-underscore --march=v32
#ld: --shared -m crislinux
#objdump: -s -T

.*:     file format elf32-cris

DYNAMIC SYMBOL TABLE:
#...
0+2a8 g    DF \.text	0+8 dsofn3
#...
0+2a4 g    DF \.text	0+ dsofn
#...
Contents of section \.rela\.plt:
 0264 44230000 0b100000 00000000           .*
Contents of section \.plt:
 0270 84e20401 7e7a3f7a 04f26ffa bf09b005  .*
 0280 00000000 00000000 00006f0d 0c000000  .*
 0290 6ffabf09 b0053f7e 00000000 bf0ed4ff  .*
 02a0 ffffb005                             .*
Contents of section \.text:
 02a4 b0050000 bfbee2ff ffffb005           .*
Contents of section \.data:
Contents of section \.dynamic:
#...
Contents of section \.got:
 2338 c0220000 00000000 00000000 96020000  .*
