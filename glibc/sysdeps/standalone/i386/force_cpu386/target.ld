/* Copyright (C) 1994, 1997 Free Software Foundation, Inc.
   Contributed by Joel Sherrill (jsherril@redstone-emh2.army.mil),
     On-Line Applications Research Corporation.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* This file contains directives for the GNU linker which are specific
to the FORCE CPU386 board.  */

MEMORY
        {
        ram : org = 0x0, l = 1M
        }

/* This value is also when the space is allocated.  If you change
this one, change the other one!!! */

heap_size = 0x20000;

SECTIONS
{
        .text 0x0 :
        {
            _text_start = ABSOLUTE(.) ;
            *(.text)
            _etext = ALIGN( 0x10 ) ;
        }
        .data ADDR( .text ) + SIZEOF( .text ):
        {
            _data_start = . ;
            *(.data)
            _edata = ALIGN( 0x10 ) ;
        }
        .bss ADDR( .data ) + SIZEOF( .data ):
        {
            _bss_start = . ;
            *(.bss)
            *(COMMON)
            heap_memory = .;
            . += 0x20000;
            _end = . ;
            __end = . ;
        }
}
