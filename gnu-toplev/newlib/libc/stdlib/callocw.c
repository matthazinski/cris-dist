/* Stub for _calloc_r, when calloc is provided by the program.
   Copyright (C) 2000 Axis Communications.  */

#ifndef _OVERRIDABLE_MALLOC
extern int _Dummy;
#else

#undef _OVERRIDABLE_MALLOC

#include <_ansi.h>
#include <reent.h>
#include <malloc.h>

_PTR _Newlib_calloc_r_actual (struct _reent *, size_t, size_t)
     __attribute__ ((weak, alias ("_Newlib_calloc_r_wrap")));

_PTR _Newlib_calloc_r (struct _reent *, size_t, size_t)
     __attribute__ ((alias ("_Newlib_calloc_r_actual")));

_PTR
_DEFUN (_Newlib_calloc_r_wrap, (ptr, n, size),
        struct _reent *ptr _AND
        size_t n _AND
        size_t size)
{
  return calloc (n, size);
}

#endif /* OVERRIDABLE_MALLOC */
