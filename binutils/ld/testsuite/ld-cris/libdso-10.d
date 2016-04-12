#source: dso-1.s
#as: --pic --no-underscore --march=v32
#ld: --shared -m crislinux
#objdump: -p -h

# Sanity check; just an empty GOT.

.*:     file format elf32-cris

Program Header:
    LOAD off    0x0+ vaddr 0x0+ paddr 0x0+ align 2\*\*13
         filesz 0x0+1ec memsz 0x0+1ec flags r-x
    LOAD off    0x0+200 vaddr 0x0+2200 paddr 0x0+2200 align 2\*\*13
         filesz 0x0+64 memsz 0x0+80 flags rw-
 DYNAMIC off    0x0+200 vaddr 0x0+2200 paddr 0x0+2200 align 2\*\*2
         filesz 0x0+58 memsz 0x0+58 flags rw-
Dynamic Section:
  HASH        0x94
  STRTAB      0x1c0
  SYMTAB      0xe0
  STRSZ       0x28
  SYMENT      0x10
private flags = 2: \[v32\]
Sections:
Idx Name          Size      VMA       LMA       File off  Algn
  0 \.hash         0+4c  0+94  0+94  0+94  2\*\*2
                  CONTENTS, ALLOC, LOAD, READONLY, DATA
  1 \.dynsym       0+e0  0+e0  0+e0  0+e0  2\*\*2
                  CONTENTS, ALLOC, LOAD, READONLY, DATA
  2 \.dynstr       0+28  0+1c0  0+1c0  0+1c0  2\*\*0
                  CONTENTS, ALLOC, LOAD, READONLY, DATA
  3 \.text         0+4  0+1e8  0+1e8  0+1e8  2\*\*0
                  CONTENTS, ALLOC, LOAD, READONLY, CODE
  4 \.data         0+  0+2200  0+2200  0+200  2\*\*0
                  CONTENTS, ALLOC, LOAD, DATA
  5 \.dynamic      0+58  0+2200  0+2200  0+200  2\*\*2
                  CONTENTS, ALLOC, LOAD, DATA
  6 \.got          0+c  0+2258  0+2258  0+258  2\*\*2
                  CONTENTS, ALLOC, LOAD, DATA
  7 \.bss          0+1c  0+2264  0+2264  0+264  2\*\*0
                  ALLOC
