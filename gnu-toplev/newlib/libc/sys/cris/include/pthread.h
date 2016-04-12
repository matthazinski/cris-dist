/* Simple fallback-include to enable thread-enabled exception support for
   libgcc, but with posix-interface to a default-dummy, so a posix library
   can optionally be linked in, which isn't possible if gthr-single.h is
   used.  No other use is supported; *DO NOT* think this gives you a valid
   pthread interface to use in your applications.

   Copyright (C) 2001 Axis Communications AB.  */

#ifndef _PTHREAD_FAKE
#define _PTHREAD_FAKE

#ifdef __cplusplus
extern "C" {
# ifndef __THROW
#  define __THROW throw ()
# endif
#else
# ifndef __THROW
#  define __THROW
# endif
#endif

typedef int pthread_once_t;
typedef unsigned int pthread_key_t;

/* This must be layout-compatible with the real type.  */
typedef struct
{
  int a, b;
  void *c;
  int d;
  struct { long int e; int f; } g;
} pthread_mutex_t;

/* This give bits equal to the real initializer.  */
#define PTHREAD_MUTEX_INITIALIZER \
  {0, 0, 0, 0, {0, 0}}

#define PTHREAD_ONCE_INIT 0

/* This isn't the right prototype, but it let's us get away with not
   defining a lot of datatypes.  */
extern int pthread_create (void) __THROW;

extern int pthread_once (pthread_once_t *, void (*) (void)) __THROW;

extern int pthread_key_create (pthread_key_t *, void (*) (void *)) __THROW;

extern int pthread_setspecific (pthread_key_t, const void *) __THROW;

extern void *pthread_getspecific (pthread_key_t) __THROW;

extern int pthread_mutex_lock (pthread_mutex_t *) __THROW;

extern int pthread_key_delete (pthread_key_t) __THROW;

extern int pthread_mutex_trylock (pthread_mutex_t *) __THROW;

extern int pthread_mutex_unlock (pthread_mutex_t *) __THROW;

#ifdef __cplusplus
}
#endif

#undef THROW

#endif /* not _PTHREAD_FAKE */
