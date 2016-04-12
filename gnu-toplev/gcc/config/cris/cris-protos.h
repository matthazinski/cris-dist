/* Definitions for GCC.  Part of the machine description for CRIS.
   Copyright (C) 1998, 1999, 2000, 2001, 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
   Contributed by Axis Communications.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Prototypes for the CRIS port.  */

#if defined(FILE) || defined(stdin) || defined(stdout) || defined(getc) || defined(putc)
#define STDIO_INCLUDED
#endif

extern void cris_conditional_register_usage PARAMS ((void));
extern int cris_simple_epilogue PARAMS ((void));
#ifdef RTX_CODE
extern const char *cris_op_str PARAMS ((rtx));
extern int cris_eligible_for_epilogue_delay PARAMS ((rtx));
extern void cris_notice_update_cc PARAMS ((rtx, rtx));
extern int cris_address_cost PARAMS ((rtx));
extern void cris_print_operand PARAMS ((FILE *, rtx, int));
extern void cris_print_operand_address PARAMS ((FILE *, rtx));
extern int cris_side_effect_mode_ok PARAMS ((enum rtx_code, rtx *, int, int,
                                             int, int, int));
extern int cris_cc0_user_requires_cmp PARAMS ((rtx));
extern rtx cris_return_addr_rtx PARAMS ((int, rtx));
extern rtx cris_split_movdx PARAMS ((rtx *));
extern int cris_legitimate_pic_operand PARAMS ((rtx));
extern enum cris_pic_symbol_type cris_pic_symbol_type_of PARAMS ((rtx));
extern int cris_valid_pic_const (rtx, int);
extern void cris_asm_output_symbol_ref PARAMS ((FILE *, rtx));
extern bool cris_output_addr_const_extra PARAMS ((FILE *, rtx));
extern int cris_got_really_used PARAMS ((void));
extern void cris_asm_output_label_ref PARAMS ((FILE *, char *));
extern void cris_target_asm_named_section
  PARAMS ((const char *, unsigned int));
extern void cris_asm_output_case_end PARAMS ((FILE *, int, rtx));
extern rtx gen_crisv32_movem_load PARAMS ((rtx, rtx, int));
extern rtx gen_crisv32_movem_store PARAMS ((rtx, rtx, int));
extern void cris_expand_pic_call_address PARAMS ((rtx *));
extern void cris_order_for_addsi3 PARAMS ((rtx *, int));

# ifdef TREE_CODE
extern rtx cris_expand_builtin_va_arg PARAMS ((tree, tree));
extern void cris_encode_section_info PARAMS ((tree));
# endif
#endif /* RTX_CODE */

#ifdef STDIO_INCLUDED
# ifdef TREE_CODE
extern void cris_asm_output_mi_thunk PARAMS ((FILE *, tree, int, tree));
extern int cris_setup_incoming_varargs
  PARAMS ((CUMULATIVE_ARGS *, enum machine_mode mode, tree, int));
# endif

extern int cris_function_ok_for_sibcall PARAMS ((tree));
extern int cris_return_address_on_stack PARAMS ((void));
extern void cris_output_sibcall_epilogue PARAMS ((void));
#endif

#ifdef GCC_C_PRAGMA_H
extern void cris_pragma_expand_mul PARAMS ((cpp_reader *));
#endif

/* Need one that returns an int; usable in expressions. */
extern int cris_fatal PARAMS ((char *));

extern void cris_override_options PARAMS ((void));

extern int cris_initial_elimination_offset PARAMS ((int, int));

extern void cris_init_expanders PARAMS ((void));

extern int cris_delay_slots_for_epilogue PARAMS ((void));

extern HOST_WIDE_INT cris_get_sr_alias_set PARAMS ((void));

extern int cris_expand_return PARAMS ((void));

extern void cris_expand_prologue PARAMS ((void));

extern void cris_expand_epilogue PARAMS ((int));

extern int cris_epilogue_eh_uses PARAMS ((int));
