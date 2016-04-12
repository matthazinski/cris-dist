/* A setjmp for CRIS.
   Copyright (C) 1993..2000 Axis Communications.  */

/* To avoid having two copies (inline and non-inline) of setjmp and
   longjmp, we do a little dirty trick here and use the header file
   itself, without "extern __inline__".  */

/* FIXME: This file should really only be used for reference, as the
   result when generating non-inline functions is somewhat dependent on
   gcc generating what we expect rather than what we describe.  An
   assembly file should be used instead.  */

#define extern
#define __inline__
#include <setjmp.h>
