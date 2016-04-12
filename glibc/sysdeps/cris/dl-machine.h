/* Machine-dependent ELF dynamic relocation inline functions.  CRIS version.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
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
   License along with the GNU C Library; see the file COPYING.LIB.  If
   not, write to the Free Software Foundation, Inc.,
   59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#ifndef dl_machine_h
#define dl_machine_h

#define ELF_MACHINE_NAME "CRIS"

#include <sys/param.h>

#ifdef __arch_v32

/* FIXME: Find all uses and intended uses and specify delay-slot there
   instead.  */
#define CALL_FN(x)				\
	"bsr " #x "\n\t"			\
	"nop"

#else /* not __arch_v32 */

#ifdef __PIC__
/* Due to limitations in the binutils CRIS port, this only works if x is
   a local symbol in the same section.  */
# define CALL_FN(x)				\
	"move.d	$pc,$r9\n\t"			\
	"add.d	" #x " - .,$r9\n\t"		\
	"jsr	$r9"
#else
# define CALL_FN(x) "jsr " #x
#endif

#endif /* not __arch_v32 */

/* Return nonzero iff ELF header is compatible with the running host.  */

static inline int
elf_machine_matches_host (const Elf32_Ehdr *ehdr)
{
  return ehdr->e_machine == EM_CRIS
#ifdef __arch_v32
    && (((ehdr->e_flags & EF_CRIS_VARIANT_MASK)
	 == EF_CRIS_VARIANT_V32)
	|| (ehdr->e_flags & EF_CRIS_VARIANT_MASK)
	 == EF_CRIS_VARIANT_COMMON_V10_V32)
#else
    && (((ehdr->e_flags & EF_CRIS_VARIANT_MASK)
	 == EF_CRIS_VARIANT_ANY_V0_V10)
	|| (ehdr->e_flags & EF_CRIS_VARIANT_MASK)
	== EF_CRIS_VARIANT_COMMON_V10_V32)
#endif
    ;
}

/* Return the link-time address of _DYNAMIC.  Conveniently, this is the
   first element of the GOT.  This must be inlined in a function which
   uses global data.  */

static inline Elf32_Addr
elf_machine_dynamic (void)
{
  /* Don't just set this to an asm variable "r0" since that's not logical
     (like, the variable is uninitialized and the register is fixed) and
     may make GCC trip over itself doing register allocation.  Yes, I'm
     paranoid.  Why do you ask?  */
  Elf32_Addr *got;

  __asm__ ("move.d $r0,%0" : "=rm" (got));
  return *got;
}

/* Return the run-time load address of the shared object.  We do it like
   m68k and i386, by taking an arbitrary local symbol, forcing a GOT entry
   for it, and peeking into the GOT table, which is set to the link-time
   file-relative symbol value (regardless of whether the target is REL or
   RELA).  We subtract this link-time file-relative value from the "local"
   value we calculate from GOT position and GOT offset.  FIXME: Perhaps
   there's some other symbol we could use, that we don't *have* to force a
   GOT entry for.  */

static inline Elf32_Addr
elf_machine_load_address (void)
{
  Elf32_Addr gotaddr_diff;
#ifdef __arch_v32
  extern char ___CRISv32_dummy[] __asm__ ("_dl_start");
  __asm__ ("addo.w _dl_start:GOT16,$r0,$acr\n\t"
	   "lapc _dl_start,%0\n\t"
	   "sub.d [$acr],%0"
	   /* For v32, we need to force GCC to have R0 loaded with
	      _GLOBAL_OFFSET_TABLE_ at this point, which might not
	      otherwise have happened in the caller.  (For v10, it's
	      loaded for non-global variables too, so we don't need
	      anything special there.)  We accomplish this by faking the
	      address of a global variable (as seen by GCC) as input to
	      the asm; that address calculation goes through the GOT.
	      Use of this function happens before we've filled in the
	      GOT, so the address itself will not be correctly
	      calculated, therefore we don't use any symbol whose
	      address may be re-used later on.  Let's just reuse the
	      _dl_start symbol, faking it as a global by renaming it as
	      another variable through an asm.  */
	   : "=r" (gotaddr_diff)
	   : "g" (___CRISv32_dummy)
	   : "acr");
#else
  __asm__ ("sub.d [$r0+_dl_start:GOT16],$r0,%0\n\t"
	   "add.d _dl_start:GOTOFF,%0" : "=r" (gotaddr_diff));
#endif
  return gotaddr_diff;
}

/* Set up the loaded object described by L so its unrelocated PLT
   entries will jump to the on-demand fixup code in dl-runtime.c.  */

static inline int
elf_machine_runtime_setup (struct link_map *l, int lazy, int profile)
{
  Elf32_Addr *got;
  extern void _dl_runtime_resolve (Elf32_Word);
  extern void _dl_runtime_profile (Elf32_Word);

  if (l->l_info[DT_JMPREL] && lazy)
    {
      /* The GOT entries for functions in the PLT have not yet been
	 filled in.  Their initial contents will, when called, arrange
	 to push an offset into the .rela.plt section, push
	 _GLOBAL_OFFSET_TABLE_[1], and then jump to
	 _GLOBAL_OFFSET_TABLE_[2].  */
      got = (Elf32_Addr *) D_PTR (l, l_info[DT_PLTGOT]);
      got[1] = (Elf32_Addr) l;	/* Identify this shared object.  */

      /* The got[2] entry contains the address of a function which gets
	 called to get the address of a so far unresolved function and
	 jump to it.  The profiling extension of the dynamic linker allows
	 to intercept the calls to collect information.  In this case we
	 don't store the address in the GOT so that all future calls also
	 end in this function.  */
      if (__builtin_expect (profile, 0))
	{
	  got[2] = (Elf32_Addr) &_dl_runtime_profile;

	  if (_dl_name_match_p (_dl_profile, l))
	    {
	      /* This is the object we are looking for.  Say that we really
		 want profiling and the timers are started.  */
	      _dl_profile_map = l;
	    }
	}
      else
	/* This function will get called to fix up the GOT entry indicated by
	   the offset on the stack, and then jump to the resolved address.  */
	got[2] = (Elf32_Addr) &_dl_runtime_resolve;
    }

  return lazy;
}

/* This code is used in dl-runtime.c to call the `fixup' function
   and then redirect to the address it returns.

   We get here with the offset into the relocation table pushed on stack,
   and the link map in MOF.  */

#ifdef __arch_v32
#define TRAMPOLINE_TEMPLATE(tramp_name, fixup_name) \
"; Trampoline for " #fixup_name	"					\n\
	.globl " #tramp_name "						\n\
	.type " #tramp_name ", @function				\n\
" #tramp_name ":							\n\
	subq	4,$sp							\n\
	move.d	$r13,[$sp]						\n\
	subq	4,$sp							\n\
	move.d	$r12,[$sp]						\n\
	subq	4,$sp							\n\
	move.d	$r11,[$sp]						\n\
	subq	4,$sp							\n\
	addoq	4*4,$sp,$acr						\n\
	move.d	$r10,[$sp]						\n\
	subq	4,$sp							\n\
	move	$mof,$r10						\n\
	move.d	$r9,[$sp]						\n\
	subq	4,$sp							\n\
	move.d	[$acr],$r11						\n\
	move	$srp,[$sp]						\n\
	" CALL_FN (fixup_name) "					\n\
	move.d	$r10,$acr						\n\
	move	[$sp+],$srp						\n\
	move.d	[$sp+],$r9						\n\
	move.d	[$sp+],$r10						\n\
	move.d	[$sp+],$r11						\n\
	move.d	[$sp+],$r12						\n\
	move.d	[$sp+],$r13						\n\
	jump	$acr							\n\
	addq	4,$sp							\n\
	.size " #tramp_name ", . - " #tramp_name "\n"
#else
#define TRAMPOLINE_TEMPLATE(tramp_name, fixup_name) \
"; Trampoline for " #fixup_name "					\n\
	.globl " #tramp_name "						\n\
	.type " #tramp_name ", @function				\n\
" #tramp_name ":							\n\
	push	$r13							\n\
	push	$r12							\n\
	push	$r11							\n\
	push	$r10							\n\
	push	$r9							\n\
	push	$srp							\n\
	move.d	[$sp+6*4],$r11						\n\
	move	$mof,$r10						\n\
	" CALL_FN (fixup_name) "					\n\
	move.d	$r10,[$sp+6*4]						\n\
	pop	$srp							\n\
	pop	$r9							\n\
	pop	$r10							\n\
	pop	$r11							\n\
	pop	$r12							\n\
	pop	$r13							\n\
	jump	[$sp+]							\n\
	.size " #tramp_name ", . - " #tramp_name "\n"
#endif /* not __arch_v32 */

#ifndef PROF
#define ELF_MACHINE_RUNTIME_TRAMPOLINE \
asm (TRAMPOLINE_TEMPLATE (_dl_runtime_resolve, fixup) \
     TRAMPOLINE_TEMPLATE (_dl_runtime_profile, profile_fixup));
#else
#define ELF_MACHINE_RUNTIME_TRAMPOLINE \
asm (TRAMPOLINE_TEMPLATE (_dl_runtime_resolve, fixup) \
     ".globl _dl_runtime_profile\n" \
     ".set _dl_runtime_profile, _dl_runtime_resolve");
#endif


/* Mask identifying addresses reserved for the user program,
   where the dynamic linker should not map anything.  */
#define ELF_MACHINE_USER_ADDRESS_MASK	0xf8000000UL

/* Initial entry point code for the dynamic linker.
   The C function `_dl_start' is the real entry point;
   its return value is the user program's entry point.  */

#ifdef __arch_v32
#define RTLD_START asm ("\
	.text								\n\
	.globl	_start							\n\
	.type	_start,@function					\n\
_start:									\n\
	move.d	$sp,$r10						\n\
	" CALL_FN (_dl_start) "						\n\
	/* FALLTHRU */							\n\
									\n\
	.globl _dl_start_user						\n\
	.type _dl_start_user,@function					\n\
_dl_start_user:								\n\
	; Save the user entry point address in R1.			\n\
	move.d	$r10,$r1						\n\
	; Point R0 at the GOT.						\n\
	lapc	_GLOBAL_OFFSET_TABLE_,$r0				\n\
	; Remember the highest stack address.				\n\
	addo.w	__libc_stack_end:GOT16,$r0,$acr				\n\
	move.d	[$acr],$r13						\n\
	move.d	$sp,[$r13]						\n\
	; See if we were run as a command with the executable file	\n\
	; name as an extra leading argument.				\n\
	addo.w	_dl_skip_args:GOT16,$r0,$acr				\n\
	move.d	[$acr],$r13						\n\
	move.d	[$r13],$r9						\n\
	; Get the original argument count				\n\
	move.d	[$sp],$r11						\n\
	; Subtract _dl_skip_args from it.				\n\
	sub.d	$r9,$r11						\n\
	; Adjust the stack pointer to skip _dl_skip_args words.		\n\
	addi	$r9.d,$sp						\n\
	; Put the new argc in place as expected by the user entry.	\n\
	move.d	$r11,[$sp]						\n\
	; Call _dl_init (struct link_map *main_map, int argc,		\n\
	;		 char **argv, char **env)			\n\
	;  env: skip scaled argc and skip stored argc and NULL		\n\
	;  at end of argv[].						\n\
	move.d	$sp,$r13						\n\
	addi	$r11.d,$r13						\n\
	addq	8,$r13							\n\
	;  argv: skip stored argc.					\n\
	move.d	$sp,$r12						\n\
	addq	4,$r12							\n\
	;  main_map: at _dl_loaded.					\n\
	addo.w	_dl_loaded:GOT16,$r0,$acr				\n\
	move.d	[$acr],$r9						\n\
	bsr	_dl_init:PLT						\n\
	move.d	[$r9],$r10						\n\
	; Pass our finalizer function to the user in R10.		\n\
	addo.w	_dl_fini:GOT16,$r0,$acr					\n\
	move.d [$acr],$r10						\n\
	; Terminate the frame-pointer.					\n\
	moveq	0,$r8							\n\
	; Jump to the user's entry point.				\n\
	jump	$r1							\n\
	; Cause SEGV if user entry returns.				\n\
	move	$r8,$srp						\n\
	.size _dl_start_user, . - _dl_start_user			\n\
	.previous");
#else  /* not __arch_v32 */
#define RTLD_START asm ("\
	.text								\n\
	.globl	_start							\n\
	.type	_start,@function					\n\
_start:									\n\
	move.d	$sp,$r10						\n\
	" CALL_FN (_dl_start) "						\n\
	/* FALLTHRU */							\n\
									\n\
	.globl _dl_start_user						\n\
	.type _dl_start_user,@function					\n\
_dl_start_user:								\n\
	; Save the user entry point address in R1.			\n\
	move.d	$r10,$r1						\n\
	; Point R0 at the GOT.						\n\
	move.d	$pc,$r0							\n\
	sub.d	.:GOTOFF,$r0						\n\
	; Remember the highest stack address.				\n\
	move.d	[$r0+__libc_stack_end:GOT16],$r13			\n\
	move.d	$sp,[$r13]						\n\
	; See if we were run as a command with the executable file	\n\
	; name as an extra leading argument.				\n\
	move.d	[$r0+_dl_skip_args:GOT16],$r13				\n\
	move.d	[$r13],$r9						\n\
	; Get the original argument count				\n\
	move.d	[$sp],$r11						\n\
	; Subtract _dl_skip_args from it.				\n\
	sub.d	$r9,$r11						\n\
	; Adjust the stack pointer to skip _dl_skip_args words.		\n\
	addi	$r9.d,$sp						\n\
	; Put the new argc in place as expected by the user entry.	\n\
	move.d	$r11,[$sp]						\n\
	; Call _dl_init (struct link_map *main_map, int argc,		\n\
	;		 char **argv, char **env)			\n\
	;  env: skip scaled argc and skip stored argc and NULL		\n\
	;  at end of argv[].						\n\
	move.d	$sp,$r13						\n\
	addi	$r11.d,$r13						\n\
	addq	8,$r13							\n\
	;  argv: skip stored argc.					\n\
	move.d	$sp,$r12						\n\
	addq	4,$r12							\n\
	;  main_map: at _dl_loaded.					\n\
	move.d	[$r0+_dl_loaded:GOT16],$r9				\n\
	move.d	[$r9],$r10						\n\
	move.d	_dl_init:PLTG,$r9					\n\
	add.d	$r0,$r9							\n\
	jsr	$r9							\n\
	; Pass our finalizer function to the user in R10.		\n\
	move.d [$r0+_dl_fini:GOT16],$r10				\n\
	; Terminate the frame-pointer.					\n\
	moveq	0,$r8							\n\
	; Cause SEGV if user entry returns.				\n\
	move	$r8,$srp						\n\
	; Jump to the user's entry point.				\n\
	jump	$r1							\n\
	.size _dl_start_user, . - _dl_start_user			\n\
	.previous");
#endif /* not __arch_v32 */

/* Nonzero if TYPE describes a relocation that should
   skip the executable when looking up the symbol value.  */
#define elf_machine_lookup_noexec_p(type) ((type) == R_CRIS_COPY)

/* Nonzero if TYPE describes relocation of a PLT entry, so
   PLT entries should not be allowed to define the value.
   Do not match relocs used for taking function addresses, or you will
   break the sameness of function addresses.  (If you can get the same
   relocs for a PLT entry in an executable as for taking a function
   address, your linker is broken.)  */
#define elf_machine_lookup_noplt_p(type) ((type) == R_CRIS_JUMP_SLOT)

/* A reloc type used for ld.so cmdline arg lookups to reject PLT entries.  */
#define ELF_MACHINE_JMP_SLOT	R_CRIS_JUMP_SLOT

/* CRIS never uses Elf32_Rel relocations.  */
#define ELF_MACHINE_NO_REL 1

/* We define an initialization functions.  This is called very early in
   _dl_sysdep_start.  */
#define DL_PLATFORM_INIT dl_platform_init ()

extern const char *_dl_platform;

static inline void __attribute__ ((unused))
dl_platform_init (void)
{
  if (_dl_platform != NULL && *_dl_platform == '\0')
    /* Avoid an empty string which would disturb us.  */
    _dl_platform = NULL;
}

static inline Elf32_Addr
elf_machine_fixup_plt (struct link_map *map, lookup_t t,
		       const Elf32_Rela *reloc,
		       Elf32_Addr *reloc_addr, Elf32_Addr value)
{
  return *reloc_addr = value;
}

/* Return the final value of a plt relocation.  */
static inline Elf32_Addr
elf_machine_plt_value (struct link_map *map, const Elf32_Rela *reloc,
		       Elf32_Addr value)
{
  return value + reloc->r_addend;
}

#endif /* !dl_machine_h */

#ifdef RESOLVE

/* Perform the relocation specified by RELOC and SYM (which is fully resolved).
   MAP is the object containing the reloc.  */

static inline void
elf_machine_rela (struct link_map *map, const Elf32_Rela *reloc,
		  const Elf32_Sym *sym, const struct r_found_version *version,
		  Elf32_Addr *const reloc_addr)
{
#ifndef RTLD_BOOTSTRAP
  /* This is defined in rtld.c, but nowhere in the static libc.a; make the
     reference weak so static programs can still link.  This declaration
     cannot be done when compiling rtld.c (i.e.  #ifdef RTLD_BOOTSTRAP)
     because rtld.c contains the common defn for _dl_rtld_map, which is
     incompatible with a weak decl in the same file.  */
  weak_extern (_dl_rtld_map);
#endif

  if (ELF32_R_TYPE (reloc->r_info) == R_CRIS_RELATIVE)
    {
#ifndef RTLD_BOOTSTRAP
      if (map != &_dl_rtld_map) /* Already done in rtld itself. */
#endif
	*reloc_addr = map->l_addr + reloc->r_addend;
    }
  else
    {
#ifndef RTLD_BOOTSTRAP
      const Elf32_Sym *const refsym = sym;
#endif
      Elf32_Addr value;
      if (sym->st_shndx != SHN_UNDEF &&
	  ELF32_ST_BIND (sym->st_info) == STB_LOCAL)
	value = map->l_addr;
      else
	{
	  value = RESOLVE (&sym, version, ELF32_R_TYPE (reloc->r_info));
	  if (sym)
	    value += sym->st_value;
	}
      value += reloc->r_addend;	/* Assume copy relocs have zero addend.  */

      switch (ELF32_R_TYPE (reloc->r_info))
	{
#ifndef RTLD_BOOTSTRAP
	case R_CRIS_COPY:
	  if (sym == NULL)
	    /* This can happen in trace mode if an object could not be
	       found.  */
	    break;
	  if (sym->st_size > refsym->st_size
	      || (_dl_verbose && sym->st_size < refsym->st_size))
	    {
	      extern char **_dl_argv;
	      const char *strtab;

	      strtab = (const void *) D_PTR (map, l_info[DT_STRTAB]);
	      _dl_error_printf ("\
%s: Symbol `%s' has different size in shared object, consider re-linking\n",
				_dl_argv[0] ?: "<program name unknown>",
				strtab + refsym->st_name);
	    }
	  memcpy (reloc_addr, (void *) value, MIN (sym->st_size,
						   refsym->st_size));
	  break;

	case R_CRIS_32:
#endif
	case R_CRIS_GLOB_DAT:
	case R_CRIS_JUMP_SLOT:
	  *reloc_addr = value;
	  break;
#ifndef RTLD_BOOTSTRAP
	case R_CRIS_8:
	  *(char *) reloc_addr = value;
	  break;
	case R_CRIS_16:
	  *(short *) reloc_addr = value;
	  break;
	case R_CRIS_8_PCREL:
	  *(char *) reloc_addr
	    = value + reloc->r_addend - (Elf32_Addr) reloc_addr - 1;
	  break;
	case R_CRIS_16_PCREL:
	  *(short *) reloc_addr
	    = value + reloc->r_addend - (Elf32_Addr) reloc_addr - 2;
	  break;
	case R_CRIS_32_PCREL:
	  *reloc_addr = value + reloc->r_addend - (Elf32_Addr) reloc_addr - 4;
	  break;
#endif
	case R_CRIS_NONE:
	  break;
#if !defined RTLD_BOOTSTRAP || defined _NDEBUG
	default:
	  _dl_reloc_bad_type (map, ELFW(R_TYPE) (reloc->r_info), 0);
	  break;
#endif
	}
    }
}

static inline void
elf_machine_lazy_rel (struct link_map *map,
		      Elf32_Addr l_addr, const Elf32_Rela *reloc)
{
  Elf32_Addr *const reloc_addr = (void *) (l_addr + reloc->r_offset);
  if (__builtin_expect (ELF32_R_TYPE (reloc->r_info), R_CRIS_JUMP_SLOT)
      == R_CRIS_JUMP_SLOT)
    *reloc_addr += l_addr;
  else
    _dl_reloc_bad_type (map, ELF32_R_TYPE (reloc->r_info), 1);
}

#endif /* RESOLVE */
