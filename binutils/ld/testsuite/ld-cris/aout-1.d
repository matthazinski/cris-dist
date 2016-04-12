#source: expfnref1.s
#as: --em=criself
#ld: --gc-sections -m criself tmpdir/aoutnondso-1.so
#objdump: -str

# Check that --gc-sections and linking an ELF and an a.out "work";
# i.e. that the GC request is just ignored.  No SEGV or such.

.*:     file format elf32-us-cris

SYMBOL TABLE:
#...
0+ g       \.startup	0+ x
#...
0+8 g       \.text	0+ expfn
#...
0+40 g       \.data	0+ expobj
#...
0+ g       \.startup	0+ __Stext

Contents of section \.startup:
 0000 3fbd0800 0000  .*
Contents of section \.init:
Contents of section .text:
 0006 0f050f05       .*
Contents of section .fini:
Contents of section \.data:
 0040 00000000       .*
Contents of section \.ctors:
Contents of section \.dtors:
