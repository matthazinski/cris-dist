/* Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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

#ifndef _FENV_H
# error "Never use <bits/fenv.h> directly; include <fenv.h> instead."
#endif


/* Define the bits representing the exception.

   Note that these are the bit positions as defined by the OSF/1
   ieee_{get,set}_control_word interface and not by the hardware fpcr.

   See the Alpha Architecture Handbook section 4.7.7.3 for details,
   but in summary, trap shadows mean the hardware register can acquire
   extra exception bits so for proper IEEE support the tracking has to
   be done in software -- in this case with kernel support.

   As to why the system call interface isn't in the same format as
   the hardware register, only those crazy folks at DEC can tell you.  */

enum
  {
#ifdef __USE_GNU
    FE_DENORMAL =	1UL << 22,
#define FE_DENORMAL	FE_DENORMAL
#endif

    FE_INEXACT =	1UL << 21,
#define FE_INEXACT	FE_INEXACT

    FE_UNDERFLOW =	1UL << 20,
#define FE_UNDERFLOW	FE_UNDERFLOW

    FE_OVERFLOW =	1UL << 19,
#define FE_OVERFLOW	FE_OVERFLOW

    FE_DIVBYZERO =	1UL << 18,
#define FE_DIVBYZERO	FE_DIVBYZERO

    FE_INVALID =	1UL << 17,
#define FE_INVALID	FE_INVALID

    FE_ALL_EXCEPT =	0x3f << 17
#define FE_ALL_EXCEPT	FE_ALL_EXCEPT
  };

/* Alpha chips support all four defined rouding modes.

   Note that code must be compiled to use dynamic rounding (/d) instructions
   to see these changes.  For gcc this is -mfp-rounding-mode=d; for DEC cc
   this is -fprm d.  The default for both is static rounding to nearest.

   These are shifted down 58 bits from the hardware fpcr because the
   functions are declared to take integers.  */

enum
  {
    FE_TOWARDZERO =	0,
#define FE_TOWARDZERO	FE_TOWARDZERO

    FE_DOWNWARD = 	1,
#define FE_DOWNWARD	FE_DOWNWARD

    FE_TONEAREST =	2,
#define FE_TONEAREST	FE_TONEAREST

    FE_UPWARD =		3,
#define FE_UPWARD	FE_UPWARD
  };

#ifdef __USE_GNU
/* On later hardware, and later kernels for earlier hardware, we can forcibly
   underflow denormal inputs and outputs.  This can speed up certain programs
   significantly, usually without affecting accuracy.  */
enum
  {
    FE_MAP_DMZ =	1UL << 12,	/* Map denorm inputs to zero */
#define FE_MAP_DMZ	FE_MAP_DMZ

    FE_MAP_UMZ =	1UL << 13,	/* Map underflowed outputs to zero */
#define FE_MAP_UMZ	FE_MAP_UMZ
  };
#endif

/* Type representing exception flags.  */
typedef unsigned long int fexcept_t;

/* Type representing floating-point environment.  */
typedef unsigned long int fenv_t;

/* If the default argument is used we use this value.  Note that due to
   architecture-specified page mappings, no user-space pointer will ever
   have its two high bits set.  Co-opt one.  */
#define FE_DFL_ENV	((__const fenv_t *) 0x8800000000000000UL)

#ifdef __USE_GNU
/* Floating-point environment where none of the exceptions are masked.  */
# define FE_NOMASK_ENV	((__const fenv_t *) 0x880000000000003eUL)

/* Floating-point environment with (processor-dependent) non-IEEE floating
   point.  In this case, mapping denormals to zero.  */
# define FE_NONIEEE_ENV ((__const fenv_t *) 0x8800000000003000UL)
#endif

/* The system calls to talk to the kernel's FP code.  */
extern unsigned long int __ieee_get_fp_control (void) __THROW;
extern void __ieee_set_fp_control (unsigned long int __value) __THROW;