/*
 * ====================================================
 * Copyright (C) 1998 by Cygnus Solutions. All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */

/*
Fast version of ldexp using Intel float instructions.

   double _f_ldexp (double x, int exp);

Function calculates x * 2 ** exp.
There is no error checking or setting of errno.
*/

	#include "i386mach.h"

	.global SYM (_f_ldexp)

SYM (_f_ldexp):
	pushl ebp
	movl esp,ebp
	fild 16(ebp)
	fldl 8(ebp)
	fscale
	fstp st1

	leave
	ret
