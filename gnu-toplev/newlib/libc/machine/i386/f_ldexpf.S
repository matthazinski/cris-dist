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
Fast version of ldexpf using Intel float instructions.

   float _f_ldexpf (float x, int exp);

Function calculates x * 2 ** exp.
There is no error checking or setting of errno. 
*/

	#include "i386mach.h"

	.global SYM (_f_ldexpf)

SYM (_f_ldexpf):
	pushl ebp
	movl esp,ebp
	fild 12(ebp)
	flds 8(ebp)
	fscale
	fstp st1

	leave
	ret
