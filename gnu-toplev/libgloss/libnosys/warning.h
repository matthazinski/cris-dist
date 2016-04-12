#ifndef __WARNING_H__
#define __WARNING_H__

#ifdef HAVE_GNU_LD
# ifdef HAVE_ELF

/* We want the .gnu.warning.SYMBOL section to be unallocated.  */
#  ifdef HAVE_ASM_PREVIOUS_DIRECTIVE
#   define __make_section_unallocated(section_string)   \
  asm(".section " section_string "; .previous");
#  elif defined (HAVE_ASM_POPSECTION_DIRECTIVE)
#   define __make_section_unallocated(section_string)   \
  asm(".pushsection " section_string "; .popsection");
#  else
#   define __make_section_unallocated(section_string)
#  endif

#  define link_warning(symbol, msg)                     \
  __make_section_unallocated (".gnu.warning." __SYMBOL_PREFIX #symbol)  \
  static const char __evoke_link_warning_##symbol[]     \
    __attribute__ ((section (".gnu.warning." __SYMBOL_PREFIX #symbol))) = msg;

#else /* !ELF */

#  define link_warning(symbol, msg)             \
  asm(".stabs \"" msg "\",30,0,0,0\n"   \
      ".stabs \"" __SYMBOL_PREFIX #symbol "\",1,0,0,0\n");
# endif
#else /* !GNULD */
/* We will never be heard; they will all die horribly.  */
# define link_warning(symbol, msg)
#endif

/* A canned warning for sysdeps/stub functions.  */
#define stub_warning(name) \
  link_warning (name, \
                "warning: " #name " is not implemented and will always fail")

#endif /* __WARNING_H__ */
