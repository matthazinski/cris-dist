/* Stubs for _malloc_r etc., when malloc etc. is provided by the program.

   Copyright (C) 2000 Axis Communications.  */

/* In order for a program to successfully provide its own malloc, realloc,
   calloc and free, we need to use those functions in the library too, or
   else the program has to provide two low-level interfaces to two
   allocators or (similar madness).  So, we need to redirect requests for
   _malloc_r (et al; _malloc_r used in examples below) to the
   user-provided malloc.

   We must be careful not provide two definitions (weak or strong) for an
   undefined symbol, because it would be linker-implementation-specific
   (or similarly ambiguous) which symbol would be picked.  Instead, we
   rely on weak symbols and aliases.

   All internally generated requests, that is internally for use within
   the library, for _malloc_r will be for the symbol _Newlib_malloc_r.
   This symbol is defined in a wrapper-function, as the symbol
   _Newlib_malloc_r_actual.  This symbol has a weak and a strong
   definition:

   - In the wrapper, _Newlib_malloc_r_actual is a weak alias to
     _Newlib_malloc_r_wrap.
   - In the newlib mallocr.c, it is a strong alias pointing to _malloc_r
     (defined in the same file), with malloc.c using _malloc_r.

   The wrapper implements _Newlib_malloc_r_wrap.

   Thus, when the program provides malloc, the wrapper function
   _Newlib_malloc_r_wrap used for internal requests, while when the malloc
   comes from newlib, _malloc_r will be called, just as before, with no
   runtime overhead.

   FIXME: FIXME: FIXME: No, that's not true.  The _Newlib_malloc_r_wrap
   stub is always called; the _Newlib_malloc_r symbol is defined as
   relative to .text, not as an alias to a weak symbol.  Bug in gas or in
   my understanding about what is possible with the a.out format.  Anyway,
   with a small runtime overhead, malloc etc. are now redefinable.  Task
   accomplished satisfactorily.  Let's get that ELF port going instead and
   hope it is the silver bullet for all object-related problems.

   Due to problems with gas (the assembler), an alias pointing to a symbol
   must be emitted in the same file as the "real" symbol; so the "strong"
   alias from _Newlib_malloc_r_actual to _malloc_r must be in mallocr.c,
   while it might seem more natural to put it in malloc.c

   We cannot trivially simplify this by folding either of _malloc_r
   _Newlib_malloc_r, _Newlib_malloc_r_actual or _Newlib_malloc_r_wrap into
   each other.  I'll try to explain without drawings:

   - _malloc_r as an externally visible symbol must have this name.
   - _Newlib_malloc_r must be distinct from _malloc_r.
   - _Newlib_malloc_r must not be defined in two objects (weak or strong),
      since it is undefined.
   - _Newlib_malloc_r_actual must not be undefined, since it is provided
     by both mallocw.o and mallocr.o.
   - _Newlib_malloc_r_wrap must be a distinct symbol, defined in only one
   object.

   Perhaps simplifications can be made anyway, or better names be found.

   One drawback is that the wrapper code is always linked in, although
   unused, and (a less drawback) a malloc is always present.
   The hope is that the wrapper code is small enough to be ignored (or
   perhaps optimized away by a smart linker).

   Note that there cannot both be requests for _malloc_r from the program
   (or another library) and a malloc provided by the program.  It can
   probably be solved with a tweak or two, but this comment is too short
   for that (and I didn't need it for the moment).  This is worse or
   different than before, though.  */

#ifndef _OVERRIDABLE_MALLOC
extern int _Dummy;
#else

#undef _OVERRIDABLE_MALLOC

#include <_ansi.h>
#include <reent.h>
#include <malloc.h>

_PTR _Newlib_malloc_r_actual (struct _reent *, size_t)
     __attribute__ ((weak, alias ("_Newlib_malloc_r_wrap")));

_PTR _Newlib_malloc_r (struct _reent *, size_t)
     __attribute__ ((alias ("_Newlib_malloc_r_actual")));

void _Newlib_free_r_actual (struct _reent *, _PTR)
     __attribute__ ((weak, alias ("_Newlib_free_r_wrap")));

void _Newlib_free_r (struct _reent *, _PTR)
     __attribute__ ((alias ("_Newlib_free_r_actual")));

_PTR
_DEFUN (_Newlib_malloc_r_wrap, (ptr, size),
        struct _reent *ptr _AND
        size_t size)
{
  return malloc (size);
}

void
_DEFUN (_Newlib_free_r_wrap, (ptr, aptr),
        struct _reent *ptr _AND
        _PTR aptr)
{
 free (aptr);
}

#endif /* OVERRIDABLE_MALLOC */
