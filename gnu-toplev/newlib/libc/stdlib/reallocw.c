/* Stub for _realloc_r, when realloc is provided by the program.
   Copyright (C) 2000 Axis Communications.  */

#ifndef _OVERRIDABLE_MALLOC
extern int _Dummy;
#else

#undef _OVERRIDABLE_MALLOC

#include <_ansi.h>
#include <reent.h>
#include <malloc.h>

_PTR _Newlib_realloc_r_actual (struct _reent *, _PTR, size_t)
     __attribute__ ((weak, alias ("_Newlib_realloc_r_wrap")));

_PTR _Newlib_realloc_r (struct _reent *, _PTR, size_t)
     __attribute__ ((alias ("_Newlib_realloc_r_actual")));

_PTR
_DEFUN (_Newlib_realloc_r_wrap, (ptr, aptr, size),
        struct _reent *ptr _AND
        _PTR aptr _AND
        size_t size)
{
  return realloc (aptr, size);
}

#endif /* OVERRIDABLE_MALLOC */
