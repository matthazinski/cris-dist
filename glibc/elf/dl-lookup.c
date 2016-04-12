/* Look up a symbol in the loaded objects.
   Copyright (C) 1995,96,97,98,99,2000,2001 Free Software Foundation, Inc.
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

#include <alloca.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ldsodefs.h>
#include "dl-hash.h"
#include <dl-machine.h>
#include <bits/libc-lock.h>

#include <assert.h>

#define VERSTAG(tag)	(DT_NUM + DT_THISPROCNUM + DT_VERSIONTAGIDX (tag))

/* We need this string more than once.  */
static const char undefined_msg[] = "undefined symbol: ";


struct sym_val
  {
    const ElfW(Sym) *s;
    struct link_map *m;
  };


#define make_string(string, rest...) \
  ({									      \
    const char *all[] = { string, ## rest };				      \
    size_t len, cnt;							      \
    char *result, *cp;							      \
									      \
    len = 1;								      \
    for (cnt = 0; cnt < sizeof (all) / sizeof (all[0]); ++cnt)		      \
      len += strlen (all[cnt]);						      \
									      \
    cp = result = alloca (len);						      \
    for (cnt = 0; cnt < sizeof (all) / sizeof (all[0]); ++cnt)		      \
      cp = __stpcpy (cp, all[cnt]);					      \
									      \
    result;								      \
  })

/* Statistics function.  */
unsigned long int _dl_num_relocations;

/* During the program run we must not modify the global data of
   loaded shared object simultanously in two threads.  Therefore we
   protect `_dl_open' and `_dl_close' in dl-close.c.

   This must be a recursive lock since the initializer function of
   the loaded object might as well require a call to this function.
   At this time it is not anymore a problem to modify the tables.  */
__libc_lock_define (extern, _dl_load_lock)


/* We have two different situations when looking up a simple: with or
   without versioning.  gcc is not able to optimize a single function
   definition serving for both purposes so we define two functions.  */
#define VERSIONED	0
#define PROTECTED	0
#include "do-lookup.h"

#define VERSIONED	1
#define PROTECTED	0
#include "do-lookup.h"


/* Add extra dependency on MAP to UNDEF_MAP.  */
static int
add_dependency (struct link_map *undef_map, struct link_map *map)
{
  struct link_map **list;
  unsigned int act;
  unsigned int i;
  int result = 0;

  /* Make sure nobody can unload the object while we are at it.  */
  __libc_lock_lock (_dl_load_lock);

  /* Determine whether UNDEF_MAP already has a reference to MAP.  First
     look in the normal dependencies.  */
  list = undef_map->l_initfini;

  for (i = 0; list[i] != NULL; ++i)
    if (list[i] == map)
      break;

  if (__builtin_expect (list[i] == NULL, 1))
    {
      /* No normal dependency.  See whether we already had to add it
	 to the special list of dynamic dependencies.  */
      list = undef_map->l_reldeps;
      act = undef_map->l_reldepsact;

      for (i = 0; i < act; ++i)
	if (list[i] == map)
	  break;

      if (i == act)
	{
	  /* The object is not yet in the dependency list.  Before we add
	     it make sure just one more time the object we are about to
	     reference is still available.  There is a brief period in
	     which the object could have been removed since we found the
	     definition.  */
	  struct link_map *runp = _dl_loaded;

	  while (runp != NULL && runp != map)
	    runp = runp->l_next;

	  if (runp != NULL)
	    {
	      /* The object is still available.  Add the reference now.  */
	      if (__builtin_expect (act >= undef_map->l_reldepsmax, 0))
		{
		  /* Allocate more memory for the dependency list.  Since
		     this can never happen during the startup phase we can
		     use `realloc'.  */
		  void *newp;

		  undef_map->l_reldepsmax += 5;
		  newp = realloc (undef_map->l_reldeps,
				  undef_map->l_reldepsmax
				  * sizeof(struct link_map *));

		  if (__builtin_expect (newp != NULL, 1))
		    undef_map->l_reldeps = (struct link_map **) newp;
		  else
		    /* Correct the addition.  */
		    undef_map->l_reldepsmax -= 5;
		}

	      /* If we didn't manage to allocate memory for the list this
		 is no fatal mistake.  We simply increment the use counter
		 of the referenced object and don't record the dependencies.
		 This means this increment can never be reverted and the
		 object will never be unloaded.  This is semantically the
		 correct behaviour.  */
	      if (__builtin_expect (act < undef_map->l_reldepsmax, 1))
		undef_map->l_reldeps[undef_map->l_reldepsact++] = map;

	      /* And increment the counter in the referenced object.  */
	      ++map->l_opencount;

	      /* Display information if we are debugging.  */
	      if (__builtin_expect (_dl_debug_mask & DL_DEBUG_FILES, 0))
		_dl_debug_printf ("\
\nfile=%s;  needed by %s (relocation dependency)\n\n",
				  map->l_name[0] ? map->l_name : _dl_argv[0],
				  undef_map->l_name[0]
				  ? undef_map->l_name : _dl_argv[0]);
	    }
	  else
	    /* Whoa, that was bad luck.  We have to search again.  */
	    result = -1;
	}
    }

  /* Release the lock.  */
  __libc_lock_unlock (_dl_load_lock);

  return result;
}


/* Search loaded objects' symbol tables for a definition of the symbol
   UNDEF_NAME.  */

lookup_t
internal_function
_dl_lookup_symbol (const char *undef_name, struct link_map *undef_map,
		   const ElfW(Sym) **ref, struct r_scope_elem *symbol_scope[],
		   int reloc_type, int explicit)
{
  const char *reference_name = undef_map ? undef_map->l_name : NULL;
  const unsigned long int hash = _dl_elf_hash (undef_name);
  struct sym_val current_value = { NULL, NULL };
  struct r_scope_elem **scope;
  int protected;
  int noexec = elf_machine_lookup_noexec_p (reloc_type);
  int noplt = elf_machine_lookup_noplt_p (reloc_type);

  ++_dl_num_relocations;

  /* Search the relevant loaded objects for a definition.  */
  for (scope = symbol_scope; *scope; ++scope)
    if (do_lookup (undef_name, hash, *ref, &current_value, *scope, 0, NULL,
		   noexec, noplt))
      {
	/* We have to check whether this would bind UNDEF_MAP to an object
	   in the global scope which was dynamically loaded.  In this case
	   we have to prevent the latter from being unloaded unless the
	   UNDEF_MAP object is also unloaded.  */
	if (__builtin_expect (current_value.m->l_global, 0)
	    && (__builtin_expect (current_value.m->l_type, lt_library)
		== lt_loaded)
	    && undef_map != current_value.m
	    /* Don't do this for explicit lookups as opposed to implicit
	       runtime lookups.  */
	    && __builtin_expect (! explicit, 1)
	    /* Add UNDEF_MAP to the dependencies.  */
	    && add_dependency (undef_map, current_value.m) < 0)
	  /* Something went wrong.  Perhaps the object we tried to reference
	     was just removed.  Try finding another definition.  */
	  return _dl_lookup_symbol (undef_name, undef_map, ref, symbol_scope,
				    reloc_type, 0);

	break;
      }

  if (__builtin_expect (current_value.s == NULL, 0))
    {
      if (*ref == NULL || ELFW(ST_BIND) ((*ref)->st_info) != STB_WEAK)
	/* We could find no value for a strong reference.  */
	/* XXX We cannot translate the messages.  */
	_dl_signal_cerror (0, (reference_name && reference_name[0]
			       ? reference_name
			       : (_dl_argv[0] ?: "<main program>")),
			   make_string (undefined_msg, undef_name));
      *ref = NULL;
      return 0;
    }

  protected = *ref && ELFW(ST_VISIBILITY) ((*ref)->st_other) == STV_PROTECTED;

  if (__builtin_expect (_dl_debug_mask & DL_DEBUG_BINDINGS, 0))
    _dl_debug_printf ("binding file %s to %s: %s symbol `%s'\n",
		      (reference_name && reference_name[0]
		       ? reference_name : (_dl_argv[0] ?: "<main program>")),
		       current_value.m->l_name[0]
		       ? current_value.m->l_name : _dl_argv[0],
		       protected ? "protected" : "normal", undef_name);

  if (__builtin_expect (protected == 0, 1))
    {
      *ref = current_value.s;
      return LOOKUP_VALUE (current_value.m);
    }
  else
    {
      /* It is very tricky. We need to figure out what value to
         return for the protected symbol */
      struct sym_val protected_value = { NULL, NULL };

      for (scope = symbol_scope; *scope; ++scope)
	if (do_lookup (undef_name, hash, *ref, &protected_value, *scope, 0,
		       NULL, 0, 1))
	  break;

      if (protected_value.s == NULL || protected_value.m == undef_map)
	{
	  *ref = current_value.s;
	  return LOOKUP_VALUE (current_value.m);
	}

      return LOOKUP_VALUE (undef_map);
    }
}


/* This function is nearly the same as `_dl_lookup_symbol' but it
   skips in the first list all objects until SKIP_MAP is found.  I.e.,
   it only considers objects which were loaded after the described
   object.  If there are more search lists the object described by
   SKIP_MAP is only skipped.  */
lookup_t
internal_function
_dl_lookup_symbol_skip (const char *undef_name,
			struct link_map *undef_map, const ElfW(Sym) **ref,
			struct r_scope_elem *symbol_scope[],
			struct link_map *skip_map)
{
  const char *reference_name = undef_map ? undef_map->l_name : NULL;
  const unsigned long int hash = _dl_elf_hash (undef_name);
  struct sym_val current_value = { NULL, NULL };
  struct r_scope_elem **scope;
  size_t i;
  int protected;

  ++_dl_num_relocations;

  /* Search the relevant loaded objects for a definition.  */
  scope = symbol_scope;
  for (i = 0; (*scope)->r_duplist[i] != skip_map; ++i)
    assert (i < (*scope)->r_nduplist);

  if (i >= (*scope)->r_nlist
	 || ! do_lookup (undef_name, hash, *ref, &current_value, *scope, i,
			 skip_map, 0, 0))
    while (*++scope)
      if (do_lookup (undef_name, hash, *ref, &current_value, *scope, 0,
		     skip_map, 0, 0))
	break;

  if (__builtin_expect (current_value.s == NULL, 0))
    {
      *ref = NULL;
      return 0;
    }

  protected = *ref && ELFW(ST_VISIBILITY) ((*ref)->st_other) == STV_PROTECTED;

  if (__builtin_expect (_dl_debug_mask & DL_DEBUG_BINDINGS, 0))
    _dl_debug_printf ("binding file %s to %s: %s symbol `%s'\n",
		       (reference_name && reference_name[0]
			? reference_name : (_dl_argv[0] ?: "<main program>")),
		       current_value.m->l_name[0]
		       ? current_value.m->l_name : _dl_argv[0],
		       protected ? "protected" : "normal", undef_name);

  if (__builtin_expect (protected == 0, 1))
    {
      *ref = current_value.s;
      return LOOKUP_VALUE (current_value.m);
    }
  else
    {
      /* It is very tricky.  We need to figure out what value to
         return for the protected symbol.  */
      struct sym_val protected_value = { NULL, NULL };

      if (i >= (*scope)->r_nlist
	  || !do_lookup (undef_name, hash, *ref, &protected_value, *scope, i,
			 skip_map, 0, 1))
	while (*++scope)
	  if (do_lookup (undef_name, hash, *ref, &protected_value, *scope, 0,
			 skip_map, 0, 1))
	    break;

      if (protected_value.s == NULL || protected_value.m == undef_map)
	{
	  *ref = current_value.s;
	  return LOOKUP_VALUE (current_value.m);
	}

      return LOOKUP_VALUE (undef_map);
    }
}


/* This function works like _dl_lookup_symbol but it takes an
   additional arguement with the version number of the requested
   symbol.

   XXX We'll see whether we need this separate function.  */
lookup_t
internal_function
_dl_lookup_versioned_symbol (const char *undef_name,
			     struct link_map *undef_map, const ElfW(Sym) **ref,
			     struct r_scope_elem *symbol_scope[],
			     const struct r_found_version *version,
			     int reloc_type, int explicit)
{
  const char *reference_name = undef_map ? undef_map->l_name : NULL;
  const unsigned long int hash = _dl_elf_hash (undef_name);
  struct sym_val current_value = { NULL, NULL };
  struct r_scope_elem **scope;
  int protected;
  int noexec = elf_machine_lookup_noexec_p (reloc_type);
  int noplt = elf_machine_lookup_noplt_p (reloc_type);

  ++_dl_num_relocations;

  /* Search the relevant loaded objects for a definition.  */
  for (scope = symbol_scope; *scope; ++scope)
    {
      int res = do_lookup_versioned (undef_name, hash, *ref, &current_value,
				     *scope, 0, version, NULL, noexec, noplt);
      if (res > 0)
	{
	  /* We have to check whether this would bind UNDEF_MAP to an object
	     in the global scope which was dynamically loaded.  In this case
	     we have to prevent the latter from being unloaded unless the
	     UNDEF_MAP object is also unloaded.  */
	  if (__builtin_expect (current_value.m->l_global, 0)
	      && (__builtin_expect (current_value.m->l_type, lt_library)
		  == lt_loaded)
	      && undef_map != current_value.m
	      /* Don't do this for explicit lookups as opposed to implicit
		 runtime lookups.  */
	      && __builtin_expect (! explicit, 1)
	      /* Add UNDEF_MAP to the dependencies.  */
	      && add_dependency (undef_map, current_value.m) < 0)
	    /* Something went wrong.  Perhaps the object we tried to reference
	       was just removed.  Try finding another definition.  */
	    return _dl_lookup_versioned_symbol (undef_name, undef_map, ref,
						symbol_scope, version,
						reloc_type, 0);

	  break;
	}

      if (__builtin_expect (res, 0) < 0)
	{
	  /* Oh, oh.  The file named in the relocation entry does not
	     contain the needed symbol.  */
	  /* XXX We cannot translate the message.  */
	  _dl_signal_cerror (0, (reference_name && reference_name[0]
				 ? reference_name
				 : (_dl_argv[0] ?: "<main program>")),
			     make_string ("symbol ", undef_name, ", version ",
					  version->name,
					  " not defined in file ",
					  version->filename,
					  " with link time reference",
					  res == -2
					  ? " (no version symbols)" : ""));
	  *ref = NULL;
	  return 0;
	}
    }

  if (__builtin_expect (current_value.s == NULL, 0))
    {
      if (*ref == NULL || ELFW(ST_BIND) ((*ref)->st_info) != STB_WEAK)
	/* We could find no value for a strong reference.  */
	/* XXX We cannot translate the message.  */
	_dl_signal_cerror (0, (reference_name && reference_name[0]
			       ? reference_name
			       : (_dl_argv[0] ?: "<main program>")),
			   make_string (undefined_msg, undef_name,
					", version ", version->name ?: NULL));
      *ref = NULL;
      return 0;
    }

  protected = *ref && ELFW(ST_VISIBILITY) ((*ref)->st_other) == STV_PROTECTED;

  if (__builtin_expect (_dl_debug_mask & DL_DEBUG_BINDINGS, 0))
    _dl_debug_printf ("binding file %s to %s: %s symbol `%s' [%s]\n",
		       (reference_name && reference_name[0]
			? reference_name : (_dl_argv[0] ?: "<main program>")),
		       current_value.m->l_name[0]
		       ? current_value.m->l_name : _dl_argv[0],
		       protected ? "protected" : "normal",
		      undef_name, version->name);

  if (__builtin_expect (protected == 0, 1))
    {
      *ref = current_value.s;
      return LOOKUP_VALUE (current_value.m);
    }
  else
    {
      /* It is very tricky. We need to figure out what value to
         return for the protected symbol */
      struct sym_val protected_value = { NULL, NULL };

      for (scope = symbol_scope; *scope; ++scope)
	if (do_lookup_versioned (undef_name, hash, *ref, &protected_value,
				 *scope, 0, version, NULL, 0, 1))
	  break;

      if (protected_value.s == NULL || protected_value.m == undef_map)
	{
	  *ref = current_value.s;
	  return LOOKUP_VALUE (current_value.m);
	}

      return LOOKUP_VALUE (undef_map);
    }
}


/* Similar to _dl_lookup_symbol_skip but takes an additional argument
   with the version we are looking for.  */
lookup_t
internal_function
_dl_lookup_versioned_symbol_skip (const char *undef_name,
				  struct link_map *undef_map,
				  const ElfW(Sym) **ref,
				  struct r_scope_elem *symbol_scope[],
				  const struct r_found_version *version,
				  struct link_map *skip_map)
{
  const char *reference_name = undef_map ? undef_map->l_name : NULL;
  const unsigned long int hash = _dl_elf_hash (undef_name);
  struct sym_val current_value = { NULL, NULL };
  struct r_scope_elem **scope;
  size_t i;
  int protected;

  ++_dl_num_relocations;

  /* Search the relevant loaded objects for a definition.  */
  scope = symbol_scope;
  for (i = 0; (*scope)->r_duplist[i] != skip_map; ++i)
    assert (i < (*scope)->r_nduplist);

  if (i >= (*scope)->r_nlist
      || ! do_lookup_versioned (undef_name, hash, *ref, &current_value,
				*scope, i, version, skip_map, 0, 0))
    while (*++scope)
      if (do_lookup_versioned (undef_name, hash, *ref, &current_value, *scope,
			       0, version, skip_map, 0, 0))
	break;

  if (__builtin_expect (current_value.s == NULL, 0))
    {
      if (*ref == NULL || ELFW(ST_BIND) ((*ref)->st_info) != STB_WEAK)
	{
	  /* We could find no value for a strong reference.  */
	  const size_t len = strlen (undef_name);
	  char buf[sizeof undefined_msg + len];
	  __mempcpy (__mempcpy (buf, undefined_msg, sizeof undefined_msg - 1),
		     undef_name, len + 1);
	  /* XXX We cannot translate the messages.  */
	  _dl_signal_cerror (0, (reference_name && reference_name[0]
				 ? reference_name
				 : (_dl_argv[0] ?: "<main program>")), buf);
	}
      *ref = NULL;
      return 0;
    }

  protected = *ref && ELFW(ST_VISIBILITY) ((*ref)->st_other) == STV_PROTECTED;

  if (__builtin_expect (_dl_debug_mask & DL_DEBUG_BINDINGS, 0))
    _dl_debug_printf ("binding file %s to %s: %s symbol `%s' [%s]\n",
		      (reference_name && reference_name[0]
		       ? reference_name : (_dl_argv[0] ?: "<main program>")),
		      current_value.m->l_name[0]
		      ? current_value.m->l_name : _dl_argv[0],
		      protected ? "protected" : "normal",
		      undef_name, version->name);

  if (__builtin_expect (protected == 0, 1))
    {
      *ref = current_value.s;
      return LOOKUP_VALUE (current_value.m);
    }
  else
    {
      /* It is very tricky. We need to figure out what value to
         return for the protected symbol */
      struct sym_val protected_value = { NULL, NULL };

      if (i >= (*scope)->r_nlist
	  || !do_lookup_versioned (undef_name, hash, *ref, &protected_value,
				   *scope, i, version, skip_map, 0, 1))
	while (*++scope)
	  if (do_lookup_versioned (undef_name, hash, *ref, &protected_value,
				   *scope, 0, version, skip_map, 0, 1))
	    break;

      if (protected_value.s == NULL || protected_value.m == undef_map)
	{
	  *ref = current_value.s;
	  return LOOKUP_VALUE (current_value.m);
	}

      return LOOKUP_VALUE (undef_map);
    }
}


/* Cache the location of MAP's hash table.  */

void
internal_function
_dl_setup_hash (struct link_map *map)
{
  Elf_Symndx *hash;
  Elf_Symndx nchain;

  if (!map->l_info[DT_HASH])
    return;
  hash = (void *)(map->l_addr + map->l_info[DT_HASH]->d_un.d_ptr);

  map->l_nbuckets = *hash++;
  nchain = *hash++;
  map->l_buckets = hash;
  hash += map->l_nbuckets;
  map->l_chain = hash;
}
