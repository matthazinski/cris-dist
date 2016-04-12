/* Call ctors and dtors from shared libraries, if any.
   Copyright (C) 1999, 2000, 2003 Axis Communications.  */

typedef void (*vfnp) (void);

/* FIXME: gcc-3.2.1 "optimized" away the guts of the _Libctors and
   _Libdtors into empty functions when the definition was here as well.
   (For that version, attribute section isn't supported for a.out.)
   Moving the definitions to after the function is a
   gcc-version-specific trick, which will fail with a later gcc version.
   Until then, implement the section attribute for a.out (.text, .data
   only).  Alternatively, make gcc not look into asm-declared consts.
   Still alternatively, emit the definitions as asm.  They absolutely
   have to end up in the .text section.  */
extern vfnp * const _Ctors asm(".$global.lib.ctors");
extern vfnp * const _Dtors asm(".$global.lib.dtors");

/* We better provide weak empty ctor and dtor lists, since they are not
   created if the main program does not have ctor/dtors.
   FIXME: Is that comment correct?  What about a program without
   ctors/dtors, but using a library with ctors/dtors?  */

static vfnp const defaultors[] = {0, 0};

extern vfnp * __CTOR_LIST__ __attribute__ ((weak, alias ("defaultors")));
extern vfnp * __DTOR_LIST__ __attribute__ ((weak, alias ("defaultors")));

void
_Libctors (void)
{
  const vfnp *firstor = _Ctors;
  const vfnp *ctors;

  /* Have to find the last ctor; they will run in opposite order as in
     the table. */
  if (firstor != 0 && *firstor != 0)
  {
    for (ctors = firstor; *ctors != 0; ctors++)
      ;

    while (--ctors != firstor)
    {
      (**ctors)();
    }

    (**ctors)();
  }
}

void _Libdtors(void)
{
  const vfnp *dtors = _Dtors;

  if (dtors)
    while (*dtors != 0)
    {
      (**dtors++) ();
    }
}

vfnp * const _Ctors asm(".$global.lib.ctors") = {0};
vfnp * const _Dtors asm(".$global.lib.dtors") = {0};
