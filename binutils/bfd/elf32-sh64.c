/* Hitachi SH64-specific support for 32-bit ELF
   Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.

   This file is part of BFD, the Binary File Descriptor library.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#define SH64_ELF

#include "bfd.h"
#include "sysdep.h"
#include "elf-bfd.h"
#include "../opcodes/sh64-opc.h"

/* Add a suffix for datalabel indirection symbols.  It must not match any
   other symbols; user symbols with or without version or other
   decoration.  It must only be used internally and not emitted by any
   means.  */
#define DATALABEL_SUFFIX " DL"

/* Used to hold data for function called through bfd_map_over_sections.  */
struct sh64_find_section_vma_data
 {
   asection *section;
   bfd_vma addr;
 };

static boolean sh64_elf_copy_private_data PARAMS ((bfd *, bfd *));
static boolean sh64_elf_merge_private_data PARAMS ((bfd *, bfd *));
static boolean sh64_elf_fake_sections PARAMS ((bfd *, Elf_Internal_Shdr *,
					      asection *));
static boolean sh64_elf_set_private_flags PARAMS ((bfd *, flagword));
static boolean sh64_elf_set_mach_from_flags PARAMS ((bfd *));
static boolean shmedia_prepare_reloc
  PARAMS ((struct bfd_link_info *, bfd *, asection *,
	   bfd_byte *, const Elf_Internal_Rela *, bfd_vma *));
static int sh64_elf_get_symbol_type PARAMS ((Elf_Internal_Sym *, int));
static boolean sh64_elf_add_symbol_hook
  PARAMS ((bfd *, struct bfd_link_info *, const Elf_Internal_Sym *,
	   const char **, flagword *, asection **, bfd_vma *));
static boolean sh64_elf_link_output_symbol_hook
  PARAMS ((bfd *, struct bfd_link_info *, const char *, Elf_Internal_Sym *,
	   asection *));
static boolean sh64_backend_section_from_shdr
  PARAMS ((bfd *, Elf_Internal_Shdr *, char *));
static void sh64_elf_final_write_processing PARAMS ((bfd *, boolean));
static boolean sh64_bfd_elf_copy_private_section_data
  PARAMS ((bfd *, asection *, bfd *, asection *));
static void sh64_find_section_for_address PARAMS ((bfd *, asection *, PTR));

/* Let elf32-sh.c handle the "bfd_" definitions, so we only have to
   intrude with an #ifndef around the function definition.  */
#define sh_elf_copy_private_data		sh64_elf_copy_private_data
#define sh_elf_merge_private_data		sh64_elf_merge_private_data
#define sh_elf_set_private_flags		sh64_elf_set_private_flags
/* Typo in elf32-sh.c (and unlinear name).  */
#define bfd_elf32_bfd_set_private_flags		sh64_elf_set_private_flags
#define sh_elf_set_mach_from_flags		sh64_elf_set_mach_from_flags

#define elf_backend_sign_extend_vma		1
#define elf_backend_fake_sections		sh64_elf_fake_sections
#define elf_backend_get_symbol_type		sh64_elf_get_symbol_type
#define elf_backend_add_symbol_hook		sh64_elf_add_symbol_hook
#define elf_backend_link_output_symbol_hook \
	sh64_elf_link_output_symbol_hook
#define elf_backend_final_write_processing 	sh64_elf_final_write_processing
#define elf_backend_section_from_shdr		sh64_backend_section_from_shdr

/* For objcopy, we need to set up sh64_elf_section_data (asection *) from
   incoming section flags.  This is otherwise done in sh64elf.em when
   linking or tc-sh64.c when assembling.  */
#define bfd_elf32_bfd_copy_private_section_data \
	sh64_bfd_elf_copy_private_section_data

/* This COFF-only function (only compiled with COFF support, making
   ELF-only chains problematic) returns true early for SH4, so let's just
   define it true here.  */
#define _bfd_sh_align_load_span(a,b,c,d,e,f,g,h,i,j) true

#ifndef ELF_ARCH
#define TARGET_BIG_SYM		bfd_elf32_sh64_vec
#define TARGET_BIG_NAME		"elf32-sh64"
#define TARGET_LITTLE_SYM	bfd_elf32_sh64l_vec
#define TARGET_LITTLE_NAME	"elf32-sh64l"
#define ELF_ARCH		bfd_arch_sh
#define ELF_MACHINE_CODE	EM_SH
#define ELF_MAXPAGESIZE		128

#define elf_symbol_leading_char '_'
#endif /* ELF_ARCH */

#define GOT_BIAS (-((long)-32768))
#define INCLUDE_SHMEDIA
#include "elf32-sh.c"

/* The type sh64_elf_crange is defined in elf/sh.h which is included in
   elf32-sh.c, hence these prototypes located after including it.  */
static int crange_qsort_cmpb PARAMS ((const void *, const void *));
static int crange_qsort_cmpl PARAMS ((const void *, const void *));
static int crange_bsearch_cmpb PARAMS ((const void *, const void *));
static int crange_bsearch_cmpl PARAMS ((const void *, const void *));
static boolean sh64_address_in_cranges
  PARAMS ((asection *cranges, bfd_vma, sh64_elf_crange *));

/* Set the SHF_SH5_ISA32 flag for ISA SHmedia code sections, and pass
   through SHT_SH5_CR_SORTED on a sorted .cranges section.  */

boolean
sh64_elf_fake_sections (output_bfd, elf_section_hdr, asect)
     bfd *output_bfd ATTRIBUTE_UNUSED;
     Elf_Internal_Shdr *elf_section_hdr;
     asection *asect;
{
  if (sh64_elf_section_data (asect) != NULL)
    elf_section_hdr->sh_flags
      |= sh64_elf_section_data (asect)->contents_flags;

  /* If this section has the SEC_SORT_ENTRIES flag set, it is a sorted
     .cranges section passing through objcopy.  */
  if ((bfd_get_section_flags (output_bfd, asect) & SEC_SORT_ENTRIES) != 0
      && strcmp (bfd_get_section_name (output_bfd, asect),
		 SH64_CRANGES_SECTION_NAME) == 0)
    elf_section_hdr->sh_type = SHT_SH5_CR_SORTED;

  return true;
}

static boolean
sh64_elf_set_mach_from_flags (abfd)
     bfd *abfd;
{
  flagword flags = elf_elfheader (abfd)->e_flags;
  asection *cranges;

  switch (flags & EF_SH_MACH_MASK)
    {
    case EF_SH5:
      /* These are fit to execute on SH5.  Just one but keep the switch
	 construct to make additions easy.  */
      bfd_default_set_arch_mach (abfd, bfd_arch_sh, bfd_mach_sh5);
      break;

    default:
      bfd_set_error (bfd_error_wrong_format);
      return false;
    }

  /* We also need to set SEC_DEBUGGING on an incoming .cranges section.
     We could have used elf_backend_section_flags if it had given us the
     section name; the bfd_section member in the header argument is not
     set at the point of the call.  FIXME: Find out whether that is by
     undocumented design or a bug.  */
  cranges = bfd_get_section_by_name (abfd, SH64_CRANGES_SECTION_NAME);
  if (cranges != NULL
      && ! bfd_set_section_flags (abfd, cranges,
				  bfd_get_section_flags (abfd, cranges)
				  | SEC_DEBUGGING))
    return false;

  return true;
}

static boolean
sh64_elf_copy_private_data (ibfd, obfd)
     bfd * ibfd;
     bfd * obfd;
{
  if (   bfd_get_flavour (ibfd) != bfd_target_elf_flavour
      || bfd_get_flavour (obfd) != bfd_target_elf_flavour)
    return true;

  BFD_ASSERT (!elf_flags_init (obfd)
	      || (elf_elfheader (obfd)->e_flags
		  == elf_elfheader (ibfd)->e_flags));

  elf_elfheader (obfd)->e_flags = elf_elfheader (ibfd)->e_flags;
  return true;
}

static boolean
sh64_elf_merge_private_data (ibfd, obfd)
     bfd *ibfd;
     bfd *obfd;
{
  flagword old_flags, new_flags;

  if (_bfd_generic_verify_endian_match (ibfd, obfd) == false)
    return false;

  if (   bfd_get_flavour (ibfd) != bfd_target_elf_flavour
      || bfd_get_flavour (obfd) != bfd_target_elf_flavour)
    return true;

  if (bfd_get_arch_size (ibfd) != bfd_get_arch_size (obfd))
    {
      const char *msg;

      if (bfd_get_arch_size (ibfd) == 32
	  && bfd_get_arch_size (obfd) == 64)
	msg = _("%s: compiled as 32-bit object and %s is 64-bit");
      else if (bfd_get_arch_size (ibfd) == 64
	       && bfd_get_arch_size (obfd) == 32)
	msg = _("%s: compiled as 64-bit object and %s is 32-bit");
      else
	msg = _("%s: object size does not match that of target %s");

      (*_bfd_error_handler) (msg, bfd_get_filename (ibfd),
			     bfd_get_filename (obfd));
      bfd_set_error (bfd_error_wrong_format);
      return false;
    }

  old_flags = elf_elfheader (obfd)->e_flags;
  new_flags = elf_elfheader (ibfd)->e_flags;
  if (! elf_flags_init (obfd))
    {
      /* This happens when ld starts out with a 'blank' output file.  */
      elf_flags_init (obfd) = true;
      elf_elfheader (obfd)->e_flags = old_flags = new_flags;
    }
  /* We don't allow linking in non-SH64 code.  */
  else if ((new_flags & EF_SH_MACH_MASK) != EF_SH5)
    {
      (*_bfd_error_handler)
	("%s: uses non-SH64 instructions while previous modules use SH64 instructions",
	 bfd_get_filename (ibfd));
      bfd_set_error (bfd_error_bad_value);
      return false;
    }

  /* I can't think of anything sane other than old_flags being EF_SH5 and
     that we need to preserve that.  */
  elf_elfheader (obfd)->e_flags = old_flags;
  return sh64_elf_set_mach_from_flags (obfd);
}

/* Handle a SH64-specific section when reading an object file.  This
   is called when elfcode.h finds a section with an unknown type.

   We only recognize SHT_SH5_CR_SORTED, on the .cranges section.  */

boolean
sh64_backend_section_from_shdr (abfd, hdr, name)
     bfd *abfd;
     Elf_Internal_Shdr *hdr;
     char *name;
{
  flagword flags = 0;

  /* We do like MIPS with a bit switch for recognized types, and returning
     false for a recognized section type with an unexpected name.  Right
     now we only have one recognized type, but that might change.  */
  switch (hdr->sh_type)
    {
    case SHT_SH5_CR_SORTED:
      if (strcmp (name, SH64_CRANGES_SECTION_NAME) != 0)
	return false;

      /* We set the SEC_SORT_ENTRIES flag so it can be passed on to
	 sh64_elf_fake_sections, keeping SHT_SH5_CR_SORTED if this object
	 passes through objcopy.  Perhaps it is brittle; the flag can
	 suddenly be used by other BFD parts, but it seems not really used
	 anywhere at the moment.  */
      flags = SEC_DEBUGGING | SEC_SORT_ENTRIES;
      break;

    default:
      return false;
    }

  if (! _bfd_elf_make_section_from_shdr (abfd, hdr, name))
    return false;

  if (flags
      && ! bfd_set_section_flags (abfd, hdr->bfd_section,
				  bfd_get_section_flags (abfd,
							 hdr->bfd_section)
				  | flags))
    return false;

  return true;
}

/* In contrast to sh64_backend_section_from_shdr, this is called for all
   sections, but only when copying sections, not when linking or
   assembling.  We need to set up the sh64_elf_section_data (asection *)
   structure for the SH64 ELF section flags to be copied correctly.  */

boolean
sh64_bfd_elf_copy_private_section_data (ibfd, isec, obfd, osec)
     bfd *ibfd;
     asection *isec;
     bfd *obfd;
     asection *osec;
{
  struct sh64_section_data *sh64_sec_data;

  if (ibfd->xvec->flavour != bfd_target_elf_flavour
      || obfd->xvec->flavour != bfd_target_elf_flavour)
    return true;

  if (! _bfd_elf_copy_private_section_data (ibfd, isec, obfd, osec))
    return false;

  sh64_sec_data = sh64_elf_section_data (isec);
  if (sh64_sec_data == NULL)
    {
      sh64_sec_data = bfd_zmalloc (sizeof (struct sh64_section_data));

      if (sh64_sec_data == NULL)
	return false;

      sh64_sec_data->contents_flags
	= (elf_section_data (isec)->this_hdr.sh_flags
	   & (SHF_SH5_ISA32 | SHF_SH5_ISA32_MIXED));

      sh64_elf_section_data (osec) = sh64_sec_data;
    }

  return true;
}

/* Function to keep SH64 specific file flags.  */

static boolean
sh64_elf_set_private_flags (abfd, flags)
     bfd *    abfd;
     flagword flags;
{
  BFD_ASSERT (! elf_flags_init (abfd)
	      || elf_elfheader (abfd)->e_flags == flags);

  elf_elfheader (abfd)->e_flags = flags;
  elf_flags_init (abfd) = true;
  return sh64_elf_set_mach_from_flags (abfd);
}

/* Called when writing out an object file to decide the type of a symbol.  */

static int
sh64_elf_get_symbol_type (elf_sym, type)
     Elf_Internal_Sym * elf_sym;
     int type;
{
  if (ELF_ST_TYPE (elf_sym->st_info) == STT_DATALABEL)
    return STT_DATALABEL;

  return type;
}

/* Hook called by the linker routine which adds symbols from an object
   file.  We must make indirect symbols for undefined symbols marked with
   STT_DATALABEL, so relocations passing them will pick up that attribute
   and neutralize STO_SH5_ISA32 found on the symbol definition.

   There is a problem, though: We want to fill in the hash-table entry for
   this symbol and signal to the caller that no further processing is
   needed.  But we don't have the index for this hash-table entry.  We
   rely here on that the current entry is the first hash-entry with NULL,
   which seems brittle.  Also, iterating over the hash-table to find that
   entry is a linear operation on the number of symbols in this input
   file, and this function should take constant time, so that's not good
   too.  Only comfort is that DataLabel references should only be found in
   hand-written assembly code and thus be rare.  FIXME: Talk maintainers
   into adding an option to elf_add_symbol_hook (preferably) for the index
   or the hash entry, alternatively adding the index to Elf_Internal_Sym
   (not so good).  */

static boolean
sh64_elf_add_symbol_hook (abfd, info, sym, namep, flagsp, secp, valp)
     bfd *abfd;
     struct bfd_link_info *info;
     const Elf_Internal_Sym *sym;
     const char **namep;
     flagword *flagsp ATTRIBUTE_UNUSED;
     asection **secp;
     bfd_vma *valp;
{
  /* We want to do this for relocatable as well as final linking.  */
  if (ELF_ST_TYPE (sym->st_info) == STT_DATALABEL
      && info->hash->creator->flavour == bfd_target_elf_flavour)
    {
      struct elf_link_hash_entry *h;

      /* For relocateable links, we register the DataLabel sym in its own
	 right, and tweak the name when it's output.  Otherwise, we make
	 an indirect symbol of it.  */
      flagword flags
	= info->relocateable || info->emitrelocations
	? BSF_GLOBAL : BSF_GLOBAL | BSF_INDIRECT;

      char *dl_name
	= bfd_malloc (strlen (*namep) + sizeof (DATALABEL_SUFFIX));
      struct elf_link_hash_entry ** sym_hash = elf_sym_hashes (abfd);

      BFD_ASSERT (sym_hash != NULL);

      /* Allocation may fail.  */
      if (dl_name == NULL)
	return false;

      strcpy (dl_name, *namep);
      strcat (dl_name, DATALABEL_SUFFIX);

      h = (struct elf_link_hash_entry *)
	bfd_link_hash_lookup (info->hash, dl_name, false, false, false);

      if (h == NULL)
	{
	  /* No previous datalabel symbol.  Make one.  */
	  if (! _bfd_generic_link_add_one_symbol (info, abfd, dl_name,
						  flags, *secp, *valp,
						  *namep, false,
						  get_elf_backend_data (abfd)->collect,
						  (struct bfd_link_hash_entry **) &h))
	    {
	      free (dl_name);
	      return false;
	    }

	  h->elf_link_hash_flags &=~ ELF_LINK_NON_ELF;
	  h->type = STT_DATALABEL;
	}
      else
	/* If a new symbol was created, it holds the allocated name.
	   Otherwise, we don't need it anymore and should deallocate it.  */
	free (dl_name);

      if (h->type != STT_DATALABEL
	  || ((info->relocateable || info->emitrelocations)
	      && h->root.type != bfd_link_hash_undefined)
	  || (! info->relocateable && !info->emitrelocations
	      && h->root.type != bfd_link_hash_indirect))
	{
	  /* Make sure we don't get confused on invalid input.  */
	  (*_bfd_error_handler)
	    (_("%s: encountered datalabel symbol in input"),
	     bfd_get_filename (abfd));
	  bfd_set_error (bfd_error_bad_value);
	  return false;
	}

      /* Now find the hash-table slot for this entry and fill it in.  */
      while (*sym_hash != NULL)
	sym_hash++;
      *sym_hash = h;

      /* Signal to caller to skip this symbol - we've handled it.  */
      *namep = NULL;
    }

  return true;
}

/* This hook function is called before the linker writes out a global
   symbol.  For relocatable links, DataLabel symbols will be present in
   linker output.  We cut off the special suffix on those symbols, so the
   right name appears in the output.

   When linking and emitting relocations, there can appear global symbols
   that are not referenced by relocs, but rather only implicitly through
   DataLabel references, a relation that is not visible to the linker.
   Since no stripping of global symbols in done when doing such linking,
   we don't need to look up and make sure to emit the main symbol for each
   DataLabel symbol.  */

boolean
sh64_elf_link_output_symbol_hook (abfd, info, cname, sym, input_sec)
     bfd *abfd ATTRIBUTE_UNUSED;
     struct bfd_link_info *info;
     const char *cname;
     Elf_Internal_Sym *sym;
     asection *input_sec ATTRIBUTE_UNUSED;
{
  char *name = (char *) cname;

  if (info->relocateable || info->emitrelocations)
    {
      if (ELF_ST_TYPE (sym->st_info) == STT_DATALABEL)
	name[strlen (name) - strlen (DATALABEL_SUFFIX)] = 0;
    }

  return true;
}

/* Check a SH64-specific reloc and put the value to relocate to into
   RELOCATION, ready to pass to _bfd_final_link_relocate.  Return FALSE if
   bad value, TRUE if ok.  */

static boolean
shmedia_prepare_reloc (info, abfd, input_section,
		       contents, rel, relocation)
     struct bfd_link_info *info;
     bfd *abfd;
     asection *input_section;
     bfd_byte *contents;
     const Elf_Internal_Rela *rel;
     bfd_vma *relocation;
{
  bfd_vma disp, dropped;

  switch (ELF32_R_TYPE (rel->r_info))
    {
    case R_SH_PT_16:
      /* Check the lowest bit of the destination field.  If it is 1, we
	 check the ISA type of the destination (i.e. the low bit of the
	 "relocation" value, and emit an error if the instruction does not
	 match).  If it is 0, we change a PTA to PTB.  There should never
	 be a PTB that should change to a PTA; that indicates a toolchain
	 error; a mismatch with GAS.  */
      {
	char *msg = NULL;
	bfd_vma insn = bfd_get_32 (abfd, contents + rel->r_offset);

	if (insn & (1 << 10))
	  {
	    /* Check matching insn and ISA (address of target).  */
	    if ((insn & SHMEDIA_PTB_BIT) != 0
		&& ((*relocation + rel->r_addend) & 1) != 0)
	      msg = _("PTB mismatch: a SHmedia address (bit 0 == 1)");
	    else if ((insn & SHMEDIA_PTB_BIT) == 0
		     && ((*relocation + rel->r_addend) & 1) == 0)
	      msg = _("PTA mismatch: a SHcompact address (bit 0 == 0)");

	    if (msg != NULL
		&& ! ((*info->callbacks->reloc_dangerous)
		      (info, msg, abfd, input_section,
		       rel->r_offset)))
	      return false;
	  }
	else
	  {
	    /* We shouldn't get here with a PTB insn and a R_SH_PT_16.  It
	       means GAS output does not match expectations; a PTA or PTB
	       expressed as such (or a PT found at assembly to be PTB)
	       would match the test above, and PT expansion with an
	       unknown destination (or when relaxing) will get us here.  */
	    if ((insn & SHMEDIA_PTB_BIT) != 0)
	      {
		(*_bfd_error_handler)
		  (_("%s: GAS error: unexpected PTB insn with R_SH_PT_16"),
		   bfd_get_filename (input_section->owner));
		return false;
	      }

	    /* Change the PTA to a PTB, if destination indicates so.  */
	    if (((*relocation + rel->r_addend) & 1) == 0)
	      bfd_put_32 (abfd, insn | SHMEDIA_PTB_BIT,
			  contents + rel->r_offset);
	  }
      }

    case R_SH_SHMEDIA_CODE:
    case R_SH_DIR5U:
    case R_SH_DIR6S:
    case R_SH_DIR6U:
    case R_SH_DIR10S:
    case R_SH_DIR10SW:
    case R_SH_DIR10SL:
    case R_SH_DIR10SQ:
    case R_SH_IMMS16:
    case R_SH_IMMU16:
    case R_SH_IMM_LOW16:
    case R_SH_IMM_LOW16_PCREL:
    case R_SH_IMM_MEDLOW16:
    case R_SH_IMM_MEDLOW16_PCREL:
    case R_SH_IMM_MEDHI16:
    case R_SH_IMM_MEDHI16_PCREL:
    case R_SH_IMM_HI16:
    case R_SH_IMM_HI16_PCREL:
    case R_SH_64:
    case R_SH_64_PCREL:
      break;

    default:
      return false;
    }

  disp = (*relocation & 0xf);
  dropped = 0;
  switch (ELF32_R_TYPE (rel->r_info))
    {
    case R_SH_DIR10SW: dropped = disp & 1; break;
    case R_SH_DIR10SL: dropped = disp & 3; break;
    case R_SH_DIR10SQ: dropped = disp & 7; break;
    }
  if (dropped != 0)
    {
      (*_bfd_error_handler)
	(_("%s: error: unaligned relocation type %d at %08x reloc %08x\n"),
	 bfd_get_filename (input_section->owner), ELF32_R_TYPE (rel->r_info),
	 (unsigned)rel->r_offset, (unsigned)relocation);
      return false;
    }

  return true;
}

/* Helper function to locate the section holding a certain address.  This
   is called via bfd_map_over_sections.  */

static void
sh64_find_section_for_address (abfd, section, data)
     bfd *abfd ATTRIBUTE_UNUSED;
     asection *section;
     PTR data;
{
  bfd_vma vma;
  bfd_size_type size;
  struct sh64_find_section_vma_data *fsec_datap
    = (struct sh64_find_section_vma_data *) data;

  /* Return if already found.  */
  if (fsec_datap->section)
    return;

  /* If this section isn't part of the addressable contents, skip it.  */
  if ((bfd_get_section_flags (abfd, section) & SEC_ALLOC) == 0)
    return;

  vma = bfd_get_section_vma (abfd, section);
  if (fsec_datap->addr < vma)
    return;

  /* FIXME: section->reloc_done isn't set properly; a generic buglet
     preventing us from using bfd_get_section_size_after_reloc.  */
  size
    = section->_cooked_size ? section->_cooked_size : section->_raw_size;

  if (fsec_datap->addr >= vma + size)
    return;

  fsec_datap->section = section;
}

/* Make sure to write out the generated entries in the .cranges section
   when doing partial linking, and set bit 0 on the entry address if it
   points to SHmedia code and write sorted .cranges entries when writing
   executables (final linking and objcopy).  */

static void
sh64_elf_final_write_processing (abfd, linker)
     bfd *   abfd;
     boolean linker ATTRIBUTE_UNUSED;
{
  bfd_vma ld_generated_cranges_size;
  asection *cranges
    = bfd_get_section_by_name (abfd, SH64_CRANGES_SECTION_NAME);

  /* If no new .cranges were added, the generic ELF linker parts will
     write it all out.  If not, we need to write them out when doing
     partial linking.  For a final link, we will sort them and write them
     all out further below.  */
  if (linker
      && cranges != NULL
      && elf_elfheader (abfd)->e_type != ET_EXEC
      && (ld_generated_cranges_size
	  = sh64_elf_section_data (cranges)->cranges_growth) != 0)
    {
      bfd_vma incoming_cranges_size
	= ((cranges->_cooked_size != 0
	    ? cranges->_cooked_size : cranges->_raw_size)
	   - ld_generated_cranges_size);

      if (! bfd_set_section_contents (abfd, cranges,
				      cranges->contents
				      + incoming_cranges_size,
				      cranges->output_offset
				      + incoming_cranges_size,
				      ld_generated_cranges_size))
	{
	  bfd_set_error (bfd_error_file_truncated);
	  (*_bfd_error_handler)
	    (_("%s: could not write out added .cranges entries"),
	     bfd_get_filename (abfd));
	}
    }

  /* Only set entry address bit 0 and sort .cranges when linking to an
     executable; never with objcopy or strip.  */
  if (linker && elf_elfheader (abfd)->e_type == ET_EXEC)
    {
      struct sh64_find_section_vma_data fsec_data;
      sh64_elf_crange dummy;

      /* For a final link, set the low bit of the entry address to
	 reflect whether or not it is a SHmedia address.
	 FIXME: Perhaps we shouldn't do this if the entry address was
	 supplied numerically, but we currently lack the infrastructure to
	 recognize that: The entry symbol, and info whether it is numeric
	 or a symbol name is kept private in the linker.  */
      fsec_data.addr = elf_elfheader (abfd)->e_entry;
      fsec_data.section = NULL;

      bfd_map_over_sections (abfd, sh64_find_section_for_address,
			     (PTR) &fsec_data);
      if (fsec_data.section
	  && (sh64_get_contents_type (fsec_data.section,
				      elf_elfheader (abfd)->e_entry,
				      &dummy) == CRT_SH5_ISA32))
	elf_elfheader (abfd)->e_entry |= 1;

      /* If we have a .cranges section, sort the entries.  */
      if (cranges != NULL)
	{
	  bfd_size_type cranges_size
	    = (cranges->_cooked_size != 0
	       ? cranges->_cooked_size : cranges->_raw_size);

	  /* We know we always have these in memory at this time.  */
	  BFD_ASSERT (cranges->contents != NULL);

	  /* The .cranges may already have been sorted in the process of
	     finding out the ISA-type of the entry address.  If not, we do
	     it here.  */
	  if (elf_section_data (cranges)->this_hdr.sh_type
	      != SHT_SH5_CR_SORTED)
	    {
	      qsort (cranges->contents, cranges_size / SH64_CRANGE_SIZE,
		     SH64_CRANGE_SIZE,
		     bfd_big_endian (cranges->owner)
		     ? crange_qsort_cmpb : crange_qsort_cmpl);
	      elf_section_data (cranges)->this_hdr.sh_type
		= SHT_SH5_CR_SORTED;
	    }

	  /* We need to write it out in whole as sorted.  */
	  if (! bfd_set_section_contents (abfd, cranges,
					  cranges->contents,
					  cranges->output_offset,
					  cranges_size))
	    {
	      bfd_set_error (bfd_error_file_truncated);
	      (*_bfd_error_handler)
		(_("%s: could not write out sorted .cranges entries"),
		 bfd_get_filename (abfd));
	    }
	}
    }
}

/* Ordering functions of a crange, for the qsort and bsearch calls and for
   different endianness.  */

static int
crange_qsort_cmpb (p1, p2)
     const PTR p1;
     const PTR p2;
{
  bfd_vma a1 = bfd_getb32 (p1);
  bfd_vma a2 = bfd_getb32 (p2);

  /* Preserve order if there's ambiguous contents.  */
  if (a1 == a2)
    return (char *) p1 - (char *) p2;

  return a1 - a2;
}

static int
crange_qsort_cmpl (p1, p2)
     const PTR p1;
     const PTR p2;
{
  bfd_vma a1 = (bfd_vma) bfd_getl32 (p1);
  bfd_vma a2 = (bfd_vma) bfd_getl32 (p2);

  /* Preserve order if there's ambiguous contents.  */
  if (a1 == a2)
    return (char *) p1 - (char *) p2;

  return a1 - a2;
}

static int
crange_bsearch_cmpb (p1, p2)
     const PTR p1;
     const PTR p2;
{
  bfd_vma a1 = *(bfd_vma *) p1;
  bfd_vma a2 = (bfd_vma) bfd_getb32 (p2);
  bfd_size_type size
    = (bfd_size_type) bfd_getb32 (SH64_CRANGE_CR_SIZE_OFFSET + (char *) p2);

  if (a1 >= a2 + size)
    return 1;
  if (a1 < a2)
    return -1;
  return 0;
}

static int
crange_bsearch_cmpl (p1, p2)
     const PTR p1;
     const PTR p2;
{
  bfd_vma a1 = *(bfd_vma *) p1;
  bfd_vma a2 = (bfd_vma) bfd_getl32 (p2);
  bfd_size_type size
    = (bfd_size_type) bfd_getl32 (SH64_CRANGE_CR_SIZE_OFFSET + (char *) p2);

  if (a1 >= a2 + size)
    return 1;
  if (a1 < a2)
    return -1;
  return 0;
}

/* Check whether a specific address is specified within a .cranges
   section.  Return FALSE if not found, and TRUE if found, and the region
   filled into RANGEP if non-NULL.  */

static boolean
sh64_address_in_cranges (cranges, addr, rangep)
     asection *cranges;
     bfd_vma addr;
     sh64_elf_crange *rangep;
{
  bfd_byte *cranges_contents;
  bfd_byte *found_rangep;
  bfd_size_type cranges_size = bfd_section_size (cranges->owner, cranges);

  /* If the size is not a multiple of the cranges entry size, then
     something is badly wrong.  */
  if ((cranges_size % SH64_CRANGE_SIZE) != 0)
    return false;

  /* If this section has relocations, then we can't do anything sane.  */
  if (bfd_get_section_flags (cranges->owner, cranges) & SEC_RELOC)
    return false;

  /* Has some kind soul (or previous call) left processed, sorted contents
     for us?  */
  if ((bfd_get_section_flags (cranges->owner, cranges) & SEC_IN_MEMORY)
      && elf_section_data (cranges)->this_hdr.sh_type == SHT_SH5_CR_SORTED)
    cranges_contents = cranges->contents;
  else
    {
      cranges_contents
	= bfd_malloc (cranges->_cooked_size == 0
		      ? cranges->_cooked_size : cranges->_raw_size);
      if (cranges_contents == NULL)
	return false;

      if (! bfd_get_section_contents (cranges->owner, cranges,
				      cranges_contents, (file_ptr) 0,
				      cranges_size))
	goto error_return;

      /* Is it sorted?  */
      if (elf_section_data (cranges)->this_hdr.sh_type
	  != SHT_SH5_CR_SORTED)
	/* Nope.  Lets sort it.  */
	qsort (cranges_contents, cranges_size / SH64_CRANGE_SIZE,
	       SH64_CRANGE_SIZE,
	       bfd_big_endian (cranges->owner)
	       ? crange_qsort_cmpb : crange_qsort_cmpl);

      /* Let's keep it around.  */
      cranges->contents = cranges_contents;
      bfd_set_section_flags (cranges->owner, cranges,
			     bfd_get_section_flags (cranges->owner, cranges)
			     | SEC_IN_MEMORY);

      /* It's sorted now.  */
      elf_section_data (cranges)->this_hdr.sh_type = SHT_SH5_CR_SORTED;
    }

  /* Try and find a matching range.  */
  found_rangep
    = bsearch (&addr, cranges_contents, cranges_size / SH64_CRANGE_SIZE, 
	       SH64_CRANGE_SIZE,
	       bfd_big_endian (cranges->owner)
	       ? crange_bsearch_cmpb : crange_bsearch_cmpl);

  /* Fill in a few return values if we found a matching range.  */
  if (found_rangep)
    {
      enum sh64_elf_cr_type cr_type
	= bfd_get_16 (cranges->owner,
		      SH64_CRANGE_CR_TYPE_OFFSET + found_rangep);
      bfd_vma cr_addr
	= bfd_get_32 (cranges->owner,
		      SH64_CRANGE_CR_ADDR_OFFSET
		      + (char *) found_rangep);
      bfd_size_type cr_size
	= bfd_get_32 (cranges->owner,
		      SH64_CRANGE_CR_SIZE_OFFSET
		      + (char *) found_rangep);

      rangep->cr_addr = cr_addr;
      rangep->cr_size = cr_size;
      rangep->cr_type = cr_type;

      return true;
    }

  /* There is a .cranges section, but it does not have a descriptor
     matching this address.  */
  return false;

error_return:
  free (cranges_contents);
  return false;
}

/* Determine what ADDR points to in SEC, and fill in a range descriptor in
   *RANGEP if it's non-NULL.  */

enum sh64_elf_cr_type
sh64_get_contents_type (sec, addr, rangep)
     asection *sec;
     bfd_vma addr;
     sh64_elf_crange *rangep;
{
  asection *cranges;

  /* Fill in the range with the boundaries of the section as a default.  */
  if (bfd_get_flavour (sec->owner) == bfd_target_elf_flavour
      && elf_elfheader (sec->owner)->e_type == ET_EXEC)
    {
      rangep->cr_addr = bfd_get_section_vma (sec->owner, sec);
      rangep->cr_size = bfd_section_size (sec->owner, sec);
      rangep->cr_type = CRT_NONE;
    }
  else
    return false;

  /* If none of the pertinent bits are set, then it's a SHcompact (or at
     least not SHmedia).  */
  if ((elf_section_data (sec)->this_hdr.sh_flags
       & (SHF_SH5_ISA32 | SHF_SH5_ISA32_MIXED)) == 0)
    {
      enum sh64_elf_cr_type cr_type
	= ((bfd_get_section_flags (sec->owner, sec) & SEC_CODE) != 0
	   ? CRT_SH5_ISA16 : CRT_DATA);
      rangep->cr_type = cr_type;
      return cr_type;
    }

  /* If only the SHF_SH5_ISA32 bit is set, then we have SHmedia.  */
  if ((elf_section_data (sec)->this_hdr.sh_flags
       & (SHF_SH5_ISA32 | SHF_SH5_ISA32_MIXED)) == SHF_SH5_ISA32)
    {
      rangep->cr_type = CRT_SH5_ISA32;
      return CRT_SH5_ISA32;
    }

  /* Otherwise, we have to look up the .cranges section.  */
  cranges = bfd_get_section_by_name (sec->owner, SH64_CRANGES_SECTION_NAME);

  if (cranges == NULL)
    /* A mixed section but there's no .cranges section.  This is probably
       bad input; it does not comply to specs.  */
    return CRT_NONE;

  /* If this call fails, we will still have CRT_NONE in rangep->cr_type
     and that will be suitable to return.  */
  sh64_address_in_cranges (cranges, addr, rangep);

  return rangep->cr_type;
}

/* This is a simpler exported interface for the benefit of gdb et al.  */

boolean
sh64_address_is_shmedia (sec, addr)
     asection *sec;
     bfd_vma addr;
{
  sh64_elf_crange dummy;
  return sh64_get_contents_type (sec, addr, &dummy) == CRT_SH5_ISA32;
}
