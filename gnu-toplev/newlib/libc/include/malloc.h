/* malloc.h -- header file for memory routines.  */

#ifndef _INCLUDE_MALLOC_H_
#define _INCLUDE_MALLOC_H_

#include <_ansi.h>
#include <sys/reent.h>

#define __need_size_t
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* This version of struct mallinfo must match the one in
   libc/stdlib/mallocr.c.  */

struct mallinfo {
  int arena;    /* total space allocated from system */
  int ordblks;  /* number of non-inuse chunks */
  int smblks;   /* unused -- always zero */
  int hblks;    /* number of mmapped regions */
  int hblkhd;   /* total space in mmapped regions */
  int usmblks;  /* unused -- always zero */
  int fsmblks;  /* unused -- always zero */
  int uordblks; /* total allocated space */
  int fordblks; /* total non-inuse space */
  int keepcost; /* top-most, releasable (via malloc_trim) space */
};	

/* The routines.  */

/* FIXME: There should be a header file shared and included by all
   compiled files in newlib but not installed, so we don't have to expose
   uncleanliness.  In the meantime, this is namespace-safe.  */
#ifdef _OVERRIDABLE_MALLOC
/* To be able to override the internally used _malloc_r with one based on
   a malloc that the program provides, we must use other names for the
   internally used functions.  */
#define _malloc_r _Newlib_malloc_r
#define _realloc_r _Newlib_realloc_r
#define _free_r _Newlib_free_r
#define _calloc_r _Newlib_calloc_r
#endif /* _OVERRIDABLE_MALLOC */

extern _PTR malloc _PARAMS ((size_t));
extern _PTR _malloc_r _PARAMS ((struct _reent *, size_t));

extern _VOID free _PARAMS ((_PTR));
extern _VOID _free_r _PARAMS ((struct _reent *, _PTR));

extern _PTR realloc _PARAMS ((_PTR, size_t));
extern _PTR _realloc_r _PARAMS ((struct _reent *, _PTR, size_t));

extern _PTR calloc _PARAMS ((size_t, size_t));
extern _PTR _calloc_r _PARAMS ((struct _reent *, size_t, size_t));

extern _PTR memalign _PARAMS ((size_t, size_t));
extern _PTR _memalign_r _PARAMS ((struct _reent *, size_t, size_t));

extern struct mallinfo mallinfo _PARAMS ((void));
extern struct mallinfo _mallinfo_r _PARAMS ((struct _reent *));

extern void malloc_stats _PARAMS ((void));
extern void _malloc_stats_r _PARAMS ((struct _reent *));

extern int mallopt _PARAMS ((int, int));
extern int _mallopt_r _PARAMS ((struct _reent *, int, int));

extern size_t malloc_usable_size _PARAMS ((_PTR));
extern size_t _malloc_usable_size_r _PARAMS ((struct _reent *, _PTR));

/* These aren't too useful on an embedded system, but we define them
   anyhow.  */

extern _PTR valloc _PARAMS ((size_t));
extern _PTR _valloc_r _PARAMS ((struct _reent *, size_t));

extern _PTR pvalloc _PARAMS ((size_t));
extern _PTR _pvalloc_r _PARAMS ((struct _reent *, size_t));

extern int malloc_trim _PARAMS ((size_t));
extern int _malloc_trim_r _PARAMS ((struct _reent *, size_t));

/* Some systems provide this, so do too for compatibility.  */

extern void cfree _PARAMS ((_PTR));

/* A compatibility routine for an earlier version of the allocator.  */

extern _VOID mstats _PARAMS ((char *));
extern _VOID _mstats_r _PARAMS ((struct _reent *, char *));

#ifdef __CYGWIN32__

/* Cygwin32 needs to be able to copy all the malloc information from
   the parent to the child.  However, cygwin32 does not normally copy
   any data in the DLL data section.  This routine handles copying
   that information.  */

extern int __malloc_copy _PARAMS ((int (*) (void *, void *, void *, int),
				   void *, int));
#endif /* __CYGWIN32 */

#ifdef __cplusplus
}
#endif

#endif /* _INCLUDE_MALLOC_H_ */
