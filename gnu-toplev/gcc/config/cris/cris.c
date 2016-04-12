/* Definitions for GCC.  Part of the machine description for CRIS.
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
   Contributed by Axis Communications.  Written by Hans-Peter Nilsson.

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

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-attr.h"
#include "flags.h"
#include "tree.h"
#include "expr.h"
#include "except.h"
#include "function.h"
#include "toplev.h"
#include "recog.h"
#include "tm_p.h"
#include "debug.h"
#include "output.h"
#include "integrate.h"
#include "ggc.h"
#include "target.h"
#include "target-def.h"

/* Usable when we have an amount to add or subtract, and want the
   optimal size of the insn.  */
#define ADDITIVE_SIZE_MODIFIER(size) \
 ((size) <= 63 ? "q" : (size) <= 255 ? "u.b" : (size) <= 65535 ? "u.w" : ".d")

#define LOSE_AND_RETURN(msgid, x)			\
  do						\
    {						\
      cris_operand_lossage (msgid, x);		\
      return;					\
    } while (0)

/* Per-function machine data.  */
struct machine_function
 {
   /* For those cases when we can't (can hardly) do it another way.  */
   int needs_return_address_on_stack;

   /* For the cases when we've decided to do it this way, but can
      reconsider when recaclulating the layout.  */
   int has_return_address_on_stack;

   /* This is the number of registers we save in the prologue due to
      varargs/stdarg.  */
   int stdarg_regs;

   int frame_layout_calculated;
   rtx srp_save_register;
 };

/* In code for output macros, this is how we know whether e.g. constant
   goes in code or in a static initializer.  */
static int in_code = 0;

/* This little fix suppresses the 'u' or 's' when '%e' in assembly
   pattern.  */
static char cris_output_insn_is_bound = 0;

/* Fix for reg_overlap_mentioned_p.  */
static int cris_reg_overlap_mentioned_p PARAMS ((rtx, rtx));

static void cris_print_base PARAMS ((rtx, FILE *));

static void cris_print_index PARAMS ((rtx, FILE *));

static void cris_output_addr_const (FILE *, rtx);

static void cris_init_machine_status PARAMS ((struct function *));

static void cris_mark_machine_status PARAMS ((struct function *));

static int cris_initial_frame_pointer_offset PARAMS ((void));

static int saved_regs_mentioned PARAMS ((rtx));

static void cris_target_asm_function_prologue
  PARAMS ((FILE *, HOST_WIDE_INT));

static void cris_target_asm_function_epilogue_1
  PARAMS ((FILE *, HOST_WIDE_INT, int, int *));

static void cris_target_asm_function_epilogue
  PARAMS ((FILE *, HOST_WIDE_INT));

static void cris_operand_lossage PARAMS ((const char *, rtx));

static void cris_normal_notice_update_cc PARAMS ((rtx));

static int cris_adjust_cost PARAMS ((rtx, rtx, rtx, int));

static int cris_reg_set_in PARAMS ((rtx reg, rtx insn));

static int cris_mem_reg_set_in PARAMS ((rtx *xp, void *insn));

static int cris_mem_in PARAMS ((rtx *xp, void *insn));

static void cris_output_addr_const PARAMS ((FILE *, rtx));

static int cris_reg_saved_in_regsave_area PARAMS ((int, int));

static void cris_calculate_frame_layout PARAMS ((int));

static int cris_movem_load_rest_p PARAMS ((rtx, int));

/* The function cris_target_asm_function_epilogue puts the last insn to
   output here.  It always fits; there won't be a symbol operand.  Used in
   delay_slots_for_epilogue and function_epilogue.  */
static char save_last[80];

/* This is the argument from the "-max-stack-stackframe=" option.  */
const char *cris_max_stackframe_str;

/* This is the argument from the "-march=" option.  */
const char *cris_cpu_str;

/* This is the argument from the "-mtune=" option.  */
const char *cris_tune_str;

/* This is the argument from the "-melinux-stacksize=" option.  */
const char *cris_elinux_stacksize_str;

/* This is the parsed result of the "-max-stack-stackframe=" option.  If
   it (still) is zero, then there was no such option given.  */
int cris_max_stackframe = 0;

/* This is the parsed result of the "-march=" option, if given.  */
int cris_cpu_version = CRIS_DEFAULT_CPU_VERSION;

/* See use in MEM_NOTRAP_P etc. in cris.h.  */
int cris_dummy;

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.dword\t"
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.quad\t"

/* We need to define these, since the 2byte, 4byte, 8byte op:s are only
   available in ELF.  These "normal" pseudos do not have any alignment
   constraints or side-effects.  */
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP TARGET_ASM_ALIGNED_HI_OP

#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP TARGET_ASM_ALIGNED_SI_OP

#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP TARGET_ASM_ALIGNED_DI_OP

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE cris_target_asm_function_prologue

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE cris_target_asm_function_epilogue

#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST cris_adjust_cost

struct gcc_target targetm = TARGET_INITIALIZER;

/* Whether to emit a compatible-nop after a label, so the target
   offset difference does not matter in compatibility mode.  */
int cris_compat_nop_after_label = 0;

/* Predicate functions.  */

/* This checks a part of an address, the one that is not a plain register
   for an addressing mode using BDAP.
   Allowed operands is either:
   a) a register
   b) a CONST operand (but not a symbol when generating PIC)
   c) a [r] or [r+] in SImode, or sign-extend from HI or QI.  */

int
cris_bdap_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  register enum rtx_code code = GET_CODE (op);

  if ((mode != SImode
       && (mode != VOIDmode || GET_MODE (op) != VOIDmode))
      || TARGET_V32
      || TARGET_V10_V32_COMPATIBLE)
    return 0;

  /* Just return whether this is a simple register or constant.  */
  if (register_operand (op, mode)
      || (CONSTANT_P (op) && (!flag_pic || cris_valid_pic_const (op, 1))))
    return 1;

  /* Is it a [r] or possibly a [r+]?  */
  if (code == MEM)
    {
      rtx tem = XEXP (op, 0);

      if (mode == SImode
	  && (register_operand (tem, SImode)
	      || (GET_CODE (tem) == POST_INC
		  && register_operand (XEXP (tem, 0), SImode))))
	return 1;
      else
	return 0;
    }

  /* Perhaps a sign-extended mem: [r].(b|w) or [r+].(b|w)?  */
  if (code == SIGN_EXTEND)
    {
      rtx tem = XEXP (op, 0);

      if (GET_CODE (tem) != MEM)
	return 0;

      tem = XEXP (tem, 0);
      if (mode == SImode
	  && (register_operand (tem, SImode)
	      || (GET_CODE (tem) == POST_INC
		  && register_operand (XEXP (tem, 0), SImode))))
	return 1;
      else
	return 0;
    }

  return 0;
}

/* This is similar to cris_bdap_operand:
   It checks a part of an address, the one that is not a plain register
   for an addressing mode using BDAP *or* BIAP.
   Allowed operands is either:
   a) a register
   b) a CONST operand (but not a symbol when generating PIC)
   c) a mult of (1, 2 or 4) and a register
   d) a [r] or [r+] in SImode, or sign-extend from HI or QI.  */

int
cris_bdap_biap_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  register enum rtx_code code = GET_CODE (op);
  rtx reg;
  rtx val;

  /* Check for bdap operand.  */
  if (cris_bdap_operand (op, mode))
    return 1;

  if (mode != SImode && (mode != VOIDmode || GET_MODE (op) != VOIDmode))
    return 0;

  /* Check that we're looking at a BIAP operand.  */
  if (code != MULT)
    return 0;

  /* Canonicalize register and multiplicand.  */
  if (GET_CODE (XEXP (op, 0)) == CONST_INT)
    {
      val = XEXP (op, 0);
      reg = XEXP (op, 1);
    }
  else
    {
      val = XEXP (op, 1);
      reg = XEXP (op, 0);
    }

  /* Check that the operands are correct after canonicalization.  */
  if (! register_operand (reg, SImode) || GET_CODE (val) != CONST_INT)
    return 0;

  /* Check that the multiplicand has a valid value.  */
  if ((code == MULT
       && (INTVAL (val) == 1 || INTVAL (val) == 2 || INTVAL (val) == 4)))
    return 1;

  return 0;
}

/* Check if MODE is same as mode for X, and X is PLUS, MINUS, IOR or
   AND or UMIN.  */

int
cris_orthogonal_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  if (mode == VOIDmode)
    mode = GET_MODE (x);

  return (GET_MODE (x) == mode
	  && (code == PLUS || code == MINUS
	      || code == IOR || code == AND || code == UMIN));
}

/* Check if MODE is same as mode for X, and X is PLUS, IOR or AND or
   UMIN.  */

int
cris_commutative_orth_op (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  if (mode == VOIDmode)
    mode = GET_MODE (x);

  return (GET_MODE (x) == mode &&
	  (code == PLUS
	   || code == IOR || code == AND || code == UMIN));
}

/* Check if MODE is same as mode for X, and X is PLUS or MINUS or UMIN.
   By the name, you might think we should include MULT.  We don't because
   it doesn't accept the same addressing modes as the others (only
   registers) and there's also the problem of handling TARGET_MUL_BUG.
   No pattern using this predicate should be enabled for !TARGET_V32 and
   PLUS.  Rather than adding conditions to all patterns in the target
   description, we do it here.  */

int
cris_operand_extend_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  if (mode == VOIDmode)
    mode = GET_MODE (x);

  return (GET_MODE (x) == mode
	  && (code == MINUS || code == UMIN || code == PLUS));
}

/* Check if MODE is same as mode for X, and X is PLUS or MINUS.  */

int
cris_additive_operand_extend_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  if (mode == VOIDmode)
    mode = GET_MODE (x);

  return (GET_MODE (x) == mode
	  && (code == PLUS || code == MINUS));
}

/* Check to see if MODE is same as mode for X, and X is SIGN_EXTEND or
   ZERO_EXTEND.  */

int
cris_extend_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  if (mode == VOIDmode)
    mode = GET_MODE (x);

  return
    (GET_MODE (x) == mode && (code == SIGN_EXTEND || code == ZERO_EXTEND));
}

/* Check to see if MODE is same as mode for X, and X is PLUS or BOUND.
   No PLUS for TARGET_V32, though.  */

int
cris_plus_or_bound_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  if (mode == VOIDmode)
    mode = GET_MODE (x);

  return
    (GET_MODE (x) == mode && (code == UMIN || code == PLUS));
}

/* Used as an operator to get a handle on a already-known-valid MEM rtx:es
   (no need to validate the address), where some address expression parts
   have their own match_operand.  */

int
cris_mem_op (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (mode == VOIDmode)
    mode = GET_MODE (x);

  return GET_MODE (x) == mode && GET_CODE (x) == MEM;
}

/* Since with -fPIC, not all symbols are valid PIC symbols or indeed
   general_operands, we have to have a predicate that matches it for the
   "movsi" expander.  */

int
cris_general_operand_or_symbol (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return general_operand (op, mode)
    || (CONSTANT_P (op)
	/* The following test is actually just an assertion.  */
	&& cris_pic_symbol_type_of (op) != cris_no_symbol);
}

/* A predicate for the anon movsi expansion, one that fits a PCREL
   operand as well as general_operand.  */

int
cris_general_operand_or_pic_source (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return general_operand (op, mode)
    || (flag_pic && cris_valid_pic_const (op, 0));
}

/* A predicate for v32 call destinations.  */

int
cris_nonmemory_operand_or_callable_symbol (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return nonmemory_operand (op, mode)
    || (GET_CODE (op) == CONST
	&& GET_CODE (XEXP (op, 0)) == UNSPEC
	&& (XINT (XEXP (op, 0), 1) == CRIS_UNSPEC_PLT_PCREL
	    || XINT (XEXP (op, 0), 1) == CRIS_UNSPEC_PCREL));
}

/* This matches a (MEM (general_operand)) or
   (MEM (cris_general_operand_or_symbol)).  The second one isn't a valid
   memory_operand, so we need this predicate to recognize call
   destinations before we change them to a PLT operand (by wrapping in
   UNSPEC CRIS_UNSPEC_PLT_(GOTREL|PCREL), for the various call expanders.  */

int
cris_mem_call_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  rtx xmem;

  if (GET_CODE (op) != MEM)
    return 0;

  if (memory_operand (op, mode))
    return 1;

  xmem = XEXP (op, 0);

  return cris_general_operand_or_symbol (xmem, GET_MODE (op));
}

/* Helper for cris_load_multiple_op and cris_ret_movem_op.  */

static int
cris_movem_load_rest_p (op, offs)
     rtx op;
     int offs;
{
  int reg_count = XVECLEN (op, 0) - offs;
  rtx src_addr;
  int i;
  rtx elt;
  int setno;
  int regno_dir = 1;
  int regno = 0;

  /* Perform a quick check so we don't blow up below.  FIXME: Adjust for
     other than (MEM reg).  */
  if (reg_count <= 1
      || GET_CODE (XVECEXP (op, 0, offs)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, offs))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, offs))) != MEM)
    return 0;

  /* Check a possible post-inc indicator.  */
  if (GET_CODE (SET_SRC (XVECEXP (op, 0, offs + 1))) == PLUS)
    {
      rtx reg = XEXP (SET_SRC (XVECEXP (op, 0, offs + 1)), 0);
      rtx inc = XEXP (SET_SRC (XVECEXP (op, 0, offs + 1)), 1);

      if (reg_count == 2
	  || !REG_P (reg)
	  || !REG_P (SET_DEST (XVECEXP (op, 0, offs + 1)))
	  || REGNO (reg) != REGNO (SET_DEST (XVECEXP (op, 0, offs + 1)))
	  || GET_CODE (inc) != CONST_INT
	  || INTVAL (inc) != (reg_count - 1) * 4)
	return 0;
      reg_count--;
      i = offs + 2;
    }
  else
    i = offs + 1;

  if (!TARGET_V32)
    {
      regno_dir = -1;
      regno = reg_count - i;
    }

  elt = XVECEXP (op, 0, offs);
  src_addr = XEXP (SET_SRC (elt), 0);

  if (GET_CODE (elt) != SET
      || GET_CODE (SET_DEST (elt)) != REG
      || GET_MODE (SET_DEST (elt)) != SImode
      || REGNO (SET_DEST (elt)) != (unsigned int) regno
      || GET_CODE (SET_SRC (elt)) != MEM
      || GET_MODE (SET_SRC (elt)) != SImode
      || !memory_address_p (SImode, src_addr))
    return 0;

  for (setno = 1; i < XVECLEN (op, 0); setno++, i++)
    {
      rtx elt = XVECEXP (op, 0, i);
      regno += regno_dir;

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || REGNO (SET_DEST (elt)) != (unsigned int) regno
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
	  || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != setno * 4)
	return 0;
    }

  return 1;
}

/* Predicate for the parallel contents in a movem from-memory.  */

int
cris_load_multiple_op (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return cris_movem_load_rest_p (op, 0);
}

/* Predicate for the special "jump R" "movem [sp+],r" pattern.  */

int
cris_ret_movem_op (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  /* The first, return-specific part, is already verified.  */
  return cris_movem_load_rest_p (op, 2);
}

/* Predicate for the parallel contents in a movem to-memory.  */

int
cris_store_multiple_op (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int reg_count = XVECLEN (op, 0);
  rtx dest_addr;
  int i;
  rtx elt;
  int setno;
  int regno_dir = 1;
  int regno = 0;

  /* Perform a quick check so we don't blow up below.  FIXME: Adjust for
     other than (MEM reg).  */
  if (reg_count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM)
    return 0;

  /* Check a possible post-inc indicator.  */
  if (GET_CODE (SET_SRC (XVECEXP (op, 0, 1))) == PLUS)
    {
      rtx reg = XEXP (SET_SRC (XVECEXP (op, 0, 1)), 0);
      rtx inc = XEXP (SET_SRC (XVECEXP (op, 0, 1)), 1);

      if (reg_count == 2
	  || !REG_P (reg)
	  || !REG_P (SET_DEST (XVECEXP (op, 0, 1)))
	  || REGNO (reg) != REGNO (SET_SRC (XVECEXP (op, 0, 1)))
	  || GET_CODE (inc) != CONST_INT
	  || INTVAL (inc) != (reg_count - 1) * 4)
	return 0;
      abort ();
      reg_count--;
      i = 2;
    }
  else
    i = 1;

  if (!TARGET_V32)
    {
      regno_dir = -1;
      regno = reg_count - i;
    }

  elt = XVECEXP (op, 0, 0);
  dest_addr = XEXP (SET_DEST (elt), 0);

  if (GET_CODE (elt) != SET
      || GET_CODE (SET_SRC (elt)) != REG
      || GET_MODE (SET_SRC (elt)) != SImode
      || REGNO (SET_SRC (elt)) != (unsigned int) regno
      || GET_CODE (SET_DEST (elt)) != MEM
      || GET_MODE (SET_DEST (elt)) != SImode
      || !memory_address_p (SImode, dest_addr))
    return 0;

  for (setno = 1; i < XVECLEN (op, 0); setno++, i++)
    {
      rtx elt = XVECEXP (op, 0, i);
      regno += regno_dir;

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || REGNO (SET_SRC (elt)) != (unsigned int) regno
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_addr)
	  || GET_CODE (XEXP (XEXP (SET_DEST (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1)) != setno * 4)
	return 0;
    }

  return 1;
}

/* The CONDITIONAL_REGISTER_USAGE worker.   */

void
cris_conditional_register_usage ()
{
  /* FIXME: This isn't nice.  We should be able to use that register for
     something else if the PIC table isn't needed.  */
  if (flag_pic)
    fixed_regs[PIC_OFFSET_TABLE_REGNUM]
      = call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;

  /* Allow use of ACR (PC in pre-V32) and tweak order.  */
  if (TARGET_V32)
    {
      static const int reg_alloc_order_v32[] = REG_ALLOC_ORDER_V32;
      unsigned int i;

      fixed_regs[CRIS_ACR_REGNUM] = 0;

      for (i = 0;
	   i < sizeof (reg_alloc_order_v32)/sizeof (reg_alloc_order_v32[0]);
	   i++)
	reg_alloc_order[i] = reg_alloc_order_v32[i];
    }

  if (TARGET_HAS_MUL_INSNS)
    fixed_regs[CRIS_MOF_REGNUM] = 0;
}

/* Given an rtx, return the text string corresponding to the CODE of X.
   Intended for use in the assembly language output section of a
   define_insn.  */

const char *
cris_op_str (x)
     rtx x;
{
  cris_output_insn_is_bound = 0;
  switch (GET_CODE (x))
    {
    case PLUS:
      return "add";
      break;

    case MINUS:
      return "sub";
      break;

    case MULT:
      /* This function is for retrieving a part of an instruction name for
	 an operator, for immediate output.  If that ever happens for
	 MULT, we need to apply TARGET_MUL_BUG in the caller.  Make sure
	 we notice.  */
      abort ();
      break;

    case DIV:
      return "div";
      break;

    case AND:
      return "and";
      break;

    case IOR:
      return "or";
      break;

    case XOR:
      return "xor";
      break;

    case NOT:
      return "not";
      break;

    case ASHIFT:
      return "lsl";
      break;

    case LSHIFTRT:
      return "lsr";
      break;

    case ASHIFTRT:
      return "asr";
      break;

    case UMIN:
      /* Used to control the sign/zero-extend character for the 'e' modifier.
	 BOUND has none.  */
      cris_output_insn_is_bound = 1;
      return "bound";
      break;

    default:
      return "Unknown operator";
      break;
  }
}

/* Emit an error message when we're in an asm, and a fatal error for
   "normal" insns.  Formatted output isn't easily implemented, since we
   use output_operand_lossage to output the actual message and handle the
   categorization of the error.  */

static void
cris_operand_lossage (msgid, op)
     const char *msgid;
     rtx op;
{
  debug_rtx (op);
  output_operand_lossage ("%s", msgid);
}

/* Print an index part of an address to file.  */

static void
cris_print_index (index, file)
     rtx index;
     FILE * file;
{
  rtx inner = XEXP (index, 0);

  /* Make the index "additive" unless we'll output a negative number, in
     which case the sign character is free (as in free beer).  */
  if (GET_CODE (index) != CONST_INT || INTVAL (index) >= 0)
    putc ('+', file);

  if (REG_P (index))
    fprintf (file, "$%s.b", reg_names[REGNO (index)]);
  else if (CONSTANT_P (index))
    cris_output_addr_const (file, index);
  else if (GET_CODE (index) == MULT)
    {
      fprintf (file, "$%s.",
	       reg_names[REGNO (XEXP (index, 0))]);

      putc (INTVAL (XEXP (index, 1)) == 2 ? 'w' : 'd', file);
    }
  else if (GET_CODE (index) == SIGN_EXTEND &&
	   GET_CODE (inner) == MEM)
    {
      rtx inner_inner = XEXP (inner, 0);

      if (GET_CODE (inner_inner) == POST_INC)
	{
	  fprintf (file, "[$%s+].",
		   reg_names[REGNO (XEXP (inner_inner, 0))]);
	  putc (GET_MODE (inner) == HImode ? 'w' : 'b', file);
	}
      else
	{
	  fprintf (file, "[$%s].", reg_names[REGNO (inner_inner)]);

	  putc (GET_MODE (inner) == HImode ? 'w' : 'b', file);
	}
    }
  else if (GET_CODE (index) == MEM)
    {
      if (GET_CODE (inner) == POST_INC)
	fprintf (file, "[$%s+].d", reg_names[REGNO (XEXP (inner, 0))]);
      else
	fprintf (file, "[$%s].d", reg_names[REGNO (inner)]);
    }
  else
    cris_operand_lossage ("unexpected index-type in cris_print_index",
			  index);
}

/* Print a base rtx of an address to file.  */

static void
cris_print_base (base, file)
     rtx base;
     FILE *file;
{
  if (REG_P (base))
    fprintf (file, "$%s", reg_names[REGNO (base)]);
  else if (GET_CODE (base) == POST_INC)
    {
      int regno = REGNO (XEXP (base, 0));
      if (regno == CRIS_ACR_REGNUM)
	abort ();
      fprintf (file, "$%s+", reg_names[regno]);
    }
  else
    cris_operand_lossage ("unexpected base-type in cris_print_base",
			  base);
}

/* Usable as a guard in expressions.  */

int
cris_fatal (arg)
     char *arg;
{
  internal_error (arg);

  /* We'll never get here; this is just to appease compilers.  */
  return 0;
}

/* Return nonzero if pic_offset_table_rtx really is used in the current
   function.  */

int
cris_got_really_used ()
{
  int got_really_used = 0;

  if (current_function_uses_pic_offset_table)
    {
      /* A reference may have been optimized out (like the abort () in
	 fde_split in unwind-dw2-fde.c, at least 3.2.1) so check that
	 it's still used.  */
      push_topmost_sequence ();
      got_really_used
	= reg_used_between_p (pic_offset_table_rtx, get_insns (),
			      NULL_RTX);
      pop_topmost_sequence ();
    }

  return got_really_used;
}

/* Return nonzero if REGNO is an ordinary register that *needs* to be
   saved together with other registers, possibly by a MOVEM instruction,
   or is saved for target-independent reasons.  There may be
   target-dependent reasons to save the register anyway; this is just a
   wrapper for a complicated conditional.  */

static int
cris_reg_saved_in_regsave_area (regno, got_really_used)
     int regno;
     int got_really_used;
{
  if (!cfun->machine->frame_layout_calculated)
    abort ();

  return
    (((regs_ever_live[regno]
       && !call_used_regs[regno])
      || (regno == (int) PIC_OFFSET_TABLE_REGNUM
	  && (got_really_used
	      /* It is saved anyway, if there would be a gap.  */
	      || (flag_pic
		  && regs_ever_live[regno + 1]
		  && !call_used_regs[regno + 1])))
      || (cfun->machine->srp_save_register != NULL
	  && regno == (int) REGNO (cfun->machine->srp_save_register)))
     && (regno != FRAME_POINTER_REGNUM || !frame_pointer_needed)
     && regno != CRIS_SRP_REGNUM)
    || (current_function_calls_eh_return
	&& (regno == EH_RETURN_DATA_REGNO (0)
	    || regno == EH_RETURN_DATA_REGNO (1)
	    || regno == EH_RETURN_DATA_REGNO (2)
	    || regno == EH_RETURN_DATA_REGNO (3)));
}

/* Textual function prologue.  */

static void
cris_target_asm_function_prologue (file, size)
     FILE *file;
     HOST_WIDE_INT size;
{
  int regno;

  /* Shorten the used name for readability.  */
  int cfoa_size = current_function_outgoing_args_size;
  int last_movem_reg = -1;
  int doing_dwarf = dwarf2out_do_frame ();
  int framesize;
  int faked_args_size = 0;
  int cfa_write_offset = 0;
  static char cfa_label[30];
  static unsigned long cfa_label_num = 0;
  int return_address_on_stack
    = regs_ever_live[CRIS_SRP_REGNUM]
    || cfun->machine->has_return_address_on_stack != 0;
  int got_really_used = cris_got_really_used ();

  /* Don't do anything if no prologues or epilogues are wanted.  For
     TARGET_V32, we expand the prologue and epilogue as RTL.  */
  if (TARGET_V32 || !TARGET_PROLOGUE_EPILOGUE)
    return;

  if (size < 0)
    abort ();

  /* Align the size to what's best for the CPU model.  */
  if (TARGET_STACK_ALIGN)
    size = TARGET_ALIGN_BY_32 ? (size + 3) & ~3 : (size + 1) & ~1;

  if (current_function_pretend_args_size)
    {
      int pretend = current_function_pretend_args_size;
      for (regno = CRIS_FIRST_ARG_REG + CRIS_MAX_ARGS_IN_REGS - 1;
	   pretend > 0;
	   regno--, pretend -= 4)
	{
	  if (TARGET_V32 || TARGET_V10_V32_COMPATIBLE)
	    /* There's no common "push" mnemonic.  */
	    fprintf (file, "\tsubq 4,$sp\n\tmove.d $%s,[$sp]\n", 
		     reg_names[regno]);
	  else
	    fprintf (file, "\tpush $%s\n", reg_names[regno]);
	  faked_args_size += 4;
	}
    }

  framesize = faked_args_size;

  if (doing_dwarf)
    {
      /* FIXME: Slightly redundant calculation, as we do the same in
	 pieces below.  This offset must be the total adjustment of the
	 stack-pointer.  We can then def_cfa call at the end of this
	 function with the current implementation of execute_cfa_insn, but
	 that wouldn't really be clean.  */

      int cfa_offset
	= faked_args_size
	+ (return_address_on_stack ? 4 : 0)
	+ (frame_pointer_needed ? 4 : 0);

      int cfa_reg;

      if (frame_pointer_needed)
	cfa_reg = FRAME_POINTER_REGNUM;
      else
	{
	  cfa_reg = STACK_POINTER_REGNUM;
	  cfa_offset += cris_initial_frame_pointer_offset ();
	}

      ASM_GENERATE_INTERNAL_LABEL (cfa_label, "LCFIT",
				   cfa_label_num++);
      dwarf2out_def_cfa (cfa_label, cfa_reg, cfa_offset);

      cfa_write_offset = - faked_args_size - 4;
    }

  /* Save SRP if not a leaf function.  */
  if (return_address_on_stack)
    {
      if (TARGET_V32 || TARGET_V10_V32_COMPATIBLE)
	/* There's no common "push" mnemonic.  */
	fprintf (file, "\tsubq 4,$sp\n\tmove $srp,[$sp]\n");
      else
	fprintf (file, "\tPush $srp\n");
      framesize += 4;

      if (doing_dwarf)
	{
	  dwarf2out_return_save (cfa_label, cfa_write_offset);
	  cfa_write_offset -= 4;
	}
    }

  /* Set up frame pointer if needed.  */
  if (frame_pointer_needed)
    {
      if (TARGET_V32 || TARGET_V10_V32_COMPATIBLE)
	/* There's no common "push" mnemonic.  */
	fprintf (file, "\tsubq 4,$sp\n\tmove.d $%s,[$sp]\n\t\
move.d $sp,$%s\n",
		 reg_names[FRAME_POINTER_REGNUM],
		 reg_names[FRAME_POINTER_REGNUM]);
      else
	fprintf (file, "\tpush $%s\n\tmove.d $sp,$%s\n",
		 reg_names[FRAME_POINTER_REGNUM],
		 reg_names[FRAME_POINTER_REGNUM]);
      framesize += 4;

      if (doing_dwarf)
	{
	  dwarf2out_reg_save (cfa_label, FRAME_POINTER_REGNUM,
			      cfa_write_offset);
	  cfa_write_offset -= 4;
	}
    }

  /* Local vars are located above saved regs.  */
  cfa_write_offset -= size;

  /* Get a contiguous sequence of registers, starting with r0, that need
     to be saved.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      if (cris_reg_saved_in_regsave_area (regno, got_really_used))
	{
	  /* Check if movem may be used for registers so far.  */
	  if (regno == last_movem_reg + 1 && !TARGET_V10_V32_COMPATIBLE)
	    /* Yes, update next expected register.  */
	    last_movem_reg++;
	  else
	    {
	      /* We cannot use movem for all registers.  We have to flush
		 any movem:ed registers we got so far.  */
	      if (last_movem_reg != -1)
		{
		  /* Handle V32 offsets for movem:d registers.  */
		  if (doing_dwarf && TARGET_V32)
		    {
		      int tmpreg;
		      for (tmpreg = last_movem_reg; tmpreg >= 0; tmpreg--)
			{
			  dwarf2out_reg_save (cfa_label, tmpreg,
					      cfa_write_offset);
			  cfa_write_offset -= 4;
			}
		    }

		  /* It is a win to use a side-effect assignment for
		     64 <= size <= 128.  But side-effect on movem was
		     not usable for CRIS v0..3.  Also only do it if
		     side-effects insns are allowed.  */
		  if ((last_movem_reg + 1) * 4 + size >= 64
		      && (last_movem_reg + 1) * 4 + size <= 128
		      && cris_cpu_version >= CRIS_CPU_SVINTO
		      && TARGET_SIDE_EFFECT_PREFIXES)
		    fprintf (file, "\tmovem $%s,[$sp=$sp-%d]\n",
			     reg_names[last_movem_reg],
			     (last_movem_reg + 1) * 4 + size);
		  else
		    {
		      /* Avoid printing multiple subsequent sub:s for sp.  */
		      fprintf (file, "\tsub%s %d,$sp\n",
			       ADDITIVE_SIZE_MODIFIER ((last_movem_reg + 1)
						       * 4 + size),
			       (last_movem_reg + 1) * 4 + size);

		      fprintf (file, "\tmovem $%s,[$sp]\n",
			       reg_names[last_movem_reg]);
		    }

		  framesize += (last_movem_reg + 1) * 4 + size;

		  if (TARGET_PDEBUG)
		    fprintf (file, "; frame %d, #regs %d, bytes %d args %d\n",
			     size,
			     last_movem_reg + 1,
			     (last_movem_reg + 1) * 4,
			     current_function_args_size);

		  last_movem_reg = -1;
		  size = 0;
		}
	      else if (size > 0)
		{
		  /* Local vars on stack, but there are no movem:s.
		     Just allocate space.  */
		  fprintf (file, "\tSub%s %d,$sp\n",
			   ADDITIVE_SIZE_MODIFIER (size),
			   size);
		  framesize += size;
		  size = 0;
		}

	      if (TARGET_V32 || TARGET_V10_V32_COMPATIBLE)
		/* There's no common "push" mnemonic.  */
		fprintf (file, "\tsubq 4,$sp\n\tmove.d $%s,[$sp]\n",
			 reg_names[regno]);
	      else
		fprintf (file, "\tPush $%s\n", reg_names[regno]);
	      framesize += 4;
	    }

	  if (doing_dwarf && (!TARGET_V32 || last_movem_reg == -1))
	    {
	      /* Except for movem with V32, registers are stored lowest
		 numbered at highest address, which matches the loop
		 order; we just need to update the write-offset.  */
	      dwarf2out_reg_save (cfa_label, regno, cfa_write_offset);
	      cfa_write_offset -= 4;
	    }
	}
    }

  /* Check after, if we can movem all registers.  This is the normal
     case.  */
  if (last_movem_reg != -1)
    {
      /* Handle V32 offsets for movem:d registers.  */
      if (doing_dwarf && TARGET_V32)
	{
	  int tmpreg;
	  for (tmpreg = last_movem_reg; tmpreg >= 0; tmpreg--)
	    {
	      dwarf2out_reg_save (cfa_label, tmpreg, cfa_write_offset);
	      cfa_write_offset -= 4;
	    }
	}

      /* Side-effect assignment on movem was not supported for CRIS v0..3,
	 and don't do it if we're asked not to.

	 The movem is already accounted for, for unwind.  */

      if ((last_movem_reg + 1) * 4 + size >= 64
	  && (last_movem_reg + 1) * 4 + size <= 128
	  && cris_cpu_version >= CRIS_CPU_SVINTO
	  && TARGET_SIDE_EFFECT_PREFIXES)
	fprintf (file, "\tmovem $%s,[$sp=$sp-%d]\n",
		 reg_names[last_movem_reg],
		 (last_movem_reg+1) * 4 + size);
      else
	{
	  /* Avoid printing multiple subsequent sub:s for sp.  FIXME:
	     Clean up the conditional expression.  */
	  fprintf (file, "\tsub%s %d,$sp\n",
		   ADDITIVE_SIZE_MODIFIER ((last_movem_reg + 1) * 4 + size),
		   (last_movem_reg + 1) * 4 + size);
	  /* To be compatible with v0..v3 means we do not use an assignment
	     addressing mode with movem.  We normally don't need that
	     anyway.  It would only be slightly more efficient for 64..128
	     bytes frame size.  */
	  fprintf (file, "\tmovem $%s,[$sp]\n", reg_names[last_movem_reg]);
	}

      framesize += (last_movem_reg + 1) * 4 + size;

      if (TARGET_PDEBUG)
	fprintf (file, "; frame %d, #regs %d, bytes %d args %d\n",
		 size,
		 last_movem_reg + 1,
		 (last_movem_reg + 1) * 4,
		 current_function_args_size);

      /* We have to put outgoing argument space after regs.  */
      if (cfoa_size)
	{
	  /* This does not need to be accounted for, for unwind.  */

	  fprintf (file, "\tSub%s %d,$sp\n",
		   ADDITIVE_SIZE_MODIFIER (cfoa_size),
		   cfoa_size);
	  framesize += cfoa_size;
	}
    }
  else if ((size + cfoa_size) > 0)
    {
      /* This does not need to be accounted for, for unwind.  */

      /* Local vars on stack, and we could not use movem.  Add a sub here.  */
      fprintf (file, "\tSub%s %d,$sp\n",
	       ADDITIVE_SIZE_MODIFIER (size + cfoa_size),
	       cfoa_size + size);
      framesize += size + cfoa_size;
    }

  /* Set up the PIC register.  */
  if (got_really_used)
    {
      if (TARGET_V32)
	abort ();
      else
	asm_fprintf (file, "\tmove.d $pc,$%s\n\tsub.d .:GOTOFF,$%s\n",
		     reg_names[PIC_OFFSET_TABLE_REGNUM],
		     reg_names[PIC_OFFSET_TABLE_REGNUM]);
    }

  if (doing_dwarf)
    ASM_OUTPUT_LABEL (file, cfa_label);

  if (TARGET_PDEBUG)
    fprintf (file,
	     "; parm #%d @ %d; frame %d, FP-SP is %d; leaf: %s%s; fp %s, outg: %d arg %d\n",
	     CRIS_MAX_ARGS_IN_REGS + 1, FIRST_PARM_OFFSET (0),
	     get_frame_size (),
	     cris_initial_frame_pointer_offset (),
	     leaf_function_p () ? "yes" : "no",
	     return_address_on_stack ? "no" :"yes",
	     frame_pointer_needed ? "yes" : "no",
	     cfoa_size, current_function_args_size);

  if (cris_max_stackframe && framesize > cris_max_stackframe)
    warning ("stackframe too big: %d bytes", framesize);
}

/* Return nonzero if there are regs mentioned in the insn that are not all
   in the call_used regs.  This is part of the decision whether an insn
   can be put in the epilogue.  */

static int
saved_regs_mentioned (x)
     rtx x;
{
  int i;
  const char *fmt;
  RTX_CODE code;

  /* Mainly stolen from refers_to_regno_p in rtlanal.c.  */

  code = GET_CODE (x);

  switch (code)
    {
    case REG:
      i = REGNO (x);
      return !call_used_regs[i];

    case SUBREG:
      /* If this is a SUBREG of a hard reg, we can see exactly which
	 registers are being modified.  Otherwise, handle normally.  */
      i = REGNO (SUBREG_REG (x));
      return !call_used_regs[i];

    default:
      ;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (saved_regs_mentioned (XEXP (x, i)))
	    return 1;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (x, i) - 1; j >=0; j--)
	    if (saved_regs_mentioned (XEXP (x, i)))
	      return 1;
	}
    }

  return 0;
}

/* Figure out if the insn may be put in the epilogue.  */

int
cris_eligible_for_epilogue_delay (insn)
     rtx insn;
{
  if (TARGET_V32)
    return 0;

  /* First of all, it must be as slottable as for a delayed branch insn.  */
  if (get_attr_slottable (insn) != SLOTTABLE_YES)
    return 0;

  /* It must not refer to the stack pointer (may be valid for some cases
     that I can't think of).  */
  if (reg_mentioned_p (stack_pointer_rtx, PATTERN (insn)))
    return 0;

  /* The frame pointer will be restored in the epilogue, before the
     "ret", so it can't be referred to.  */
  if (frame_pointer_needed
      && reg_mentioned_p (frame_pointer_rtx, PATTERN (insn)))
    return 0;

  /* All saved regs are restored before the delayed insn.
     This means that we cannot have any instructions that mention the
     registers that are restored by the epilogue.  */
  if (saved_regs_mentioned (PATTERN (insn)))
    return 0;

  if (TARGET_V10_V32_COMPATIBLE)
    /* Sorry, we don't have a delay slot in this case.  */
    return 0;

  /* It seems to be ok.  */
  return 1;
}

/* Return the number of delay-slots in the epilogue: return 1 if it
   contains "ret", else 0.  */

int
cris_delay_slots_for_epilogue ()
{
  int has_delay_slot = 0;

  /* Check if we use a return insn, which we only do for leaf functions.
     Else there is no slot to fill.  */
  if (TARGET_V32
      || regs_ever_live[CRIS_SRP_REGNUM]
      || cfun->machine->needs_return_address_on_stack != 0)
    return 0;

  /* By calling function_epilogue with the same parameters as from gcc
     we can get info about if the epilogue can fill the delay-slot by itself.
     If it is filled from the epilogue, then the corresponding string
     is in save_last.
      This depends on that the "size" argument to function_epilogue
     always is get_frame_size.
     FIXME:  Kludgy.  At least make it a separate function that is not
     misnamed or abuses the stream parameter.  */
  cris_target_asm_function_epilogue_1 (NULL, get_frame_size (), 0,
				       &has_delay_slot);
  if (has_delay_slot)
    return 1;
  return 0;
}

/* Textual function epilogue.  When file is NULL, it serves doubly as
   a test for whether the epilogue can fill any "ret" delay-slots by
   itself by storing the delay insn in save_last.  */

static void
cris_target_asm_function_epilogue_1 (file, size, for_sibcall, has_delay_slot)
     FILE *file;
     HOST_WIDE_INT size;
     int for_sibcall;
     int *has_delay_slot;
{
  int regno;
  int last_movem_reg = -1;
  rtx insn = get_last_insn ();
  int argspace_offset = current_function_outgoing_args_size;
  int pretend =	 current_function_pretend_args_size;
  int return_address_on_stack
    = regs_ever_live[CRIS_SRP_REGNUM]
    || cfun->machine->has_return_address_on_stack != 0;
  int got_really_used = cris_got_really_used ();

  if (TARGET_V32)
    return;

  save_last[0] = 0;

  if (file && !TARGET_PROLOGUE_EPILOGUE)
    return;

  if (TARGET_PDEBUG && file)
    fprintf (file, ";;\n");

  /* Align byte count of stack frame.  */
  if (TARGET_STACK_ALIGN)
    size = TARGET_ALIGN_BY_32 ? (size + 3) & ~3 : (size + 1) & ~1;

  /* If the last insn was a BARRIER, we don't have to write any code,
     then all returns were covered by "return" insns.  */
  if (GET_CODE (insn) == NOTE)
    insn = prev_nonnote_insn (insn);
  if (!for_sibcall
      && insn != NULL
      && (GET_CODE (insn) == BARRIER
	  /* We must make sure that the insn really is a "return" and
	     not a conditional branch.  Try to match the return exactly,
	     and if it doesn't match, assume it is a conditional branch
	     (and output an epilogue).  */
	  || (GET_CODE (insn) == JUMP_INSN
	      && GET_CODE (PATTERN (insn)) == RETURN)))
    {
      if (TARGET_PDEBUG && file)
	fprintf (file, ";;;;;\n");
      return;
    }

  /* Check how many saved regs we can movem.  They start at r0 and must
     be contiguous.  */
  for (regno = 0;
       regno < FIRST_PSEUDO_REGISTER;
       regno++)
    if (cris_reg_saved_in_regsave_area (regno, got_really_used))
      {
	if (regno == last_movem_reg + 1 && !TARGET_V10_V32_COMPATIBLE)
	  last_movem_reg++;
	else
	  break;
      }

  for (regno = FIRST_PSEUDO_REGISTER - 1;
       regno > last_movem_reg;
       regno--)
    if (cris_reg_saved_in_regsave_area (regno, got_really_used))
      {
	if (argspace_offset)
	  {
	    /* There is an area for outgoing parameters located before
	       the saved registers.  We have to adjust for that.  */
	    if (file)
	      fprintf (file, "\tAdd%s %d,$sp\n",
		       ADDITIVE_SIZE_MODIFIER (argspace_offset),
		       argspace_offset);

	    /* Make sure we only do this once.  */
	    argspace_offset = 0;
	  }

	/* Flush previous non-movem:ed registers.  */
	if (*save_last && file)
	  fprintf (file, save_last);
	sprintf (save_last, "\tmove.d [$sp+],$%s\n", reg_names[regno]);
      }

  if (last_movem_reg != -1)
    {
      if (argspace_offset)
	{
	  /* Adjust for the outgoing parameters area, if that's not
	     handled yet.  */
	  if (*save_last && file)
	    {
	      fprintf (file, save_last);
	      *save_last = 0;
	    }

	  if (file)
	    fprintf (file, "\tAdd%s %d,$sp\n",
		     ADDITIVE_SIZE_MODIFIER (argspace_offset),
		     argspace_offset);
	  argspace_offset = 0;
	}
      /* Flush previous non-movem:ed registers.  */
      else if (*save_last && file)
	fprintf (file, save_last);
      sprintf (save_last, "\tmovem [$sp+],$%s\n", reg_names[last_movem_reg]);
    }

  /* Restore frame pointer if necessary.  */
  if (frame_pointer_needed)
    {
      if (*save_last && file)
	fprintf (file, save_last);

      if (file)
	fprintf (file, "\tmove.d $%s,$sp\n",
		 reg_names[FRAME_POINTER_REGNUM]);
      sprintf (save_last, "\tmove.d [$sp+],$%s\n",
	       reg_names[FRAME_POINTER_REGNUM]);
    }
  else
    {
      /* If there was no frame-pointer to restore sp from, we must
	 explicitly deallocate local variables.  */

      /* Handle space for outgoing parameters that hasn't been handled
	 yet.  */
      size += argspace_offset;

      if (size)
	{
	  if (*save_last && file)
	    fprintf (file, save_last);

	  sprintf (save_last, "\tadd%s %d,$sp\n",
		   ADDITIVE_SIZE_MODIFIER (size), size);
	}

      /* If the size was not in the range for a "quick", we must flush
	 it here.  */
      if (size > 63)
	{
	  if (file)
	    fprintf (file, save_last);
	  *save_last = 0;
	}
    }

  /* If this function has no pushed register parameters
     (stdargs/varargs), and if it is not a leaf function, then we can
     just jump-return here.  */
  if (return_address_on_stack && pretend == 0)
    {
      /* Whatever we do, we'll first need to flush any saved insn,
	 because they all touch the stack or stack-pointer, because
	 whatever we do, we'll first need to access the stack.  */
      if (*save_last && file)
	fprintf (file, save_last);
      *save_last = 0;

      if (TARGET_V10_V32_COMPATIBLE)
	{
	  /* Must jump via register; a call-clobbered one.  We shouldn't
	     *generally* use R9, R10 or R11 since they're used for
	     return values.  R13 is generally safe.  It fails for one
	     case: where we return a non-struct larger than 12 bytes,
	     for example a DCmode (complex double).  Emit compile-time
	     error for that instead of generating faulty code.  Note
	     that the jump *may* have a delay slot.  */
	  if (file == NULL)
	    ;
	  else if (!aggregate_value_p (TREE_TYPE (TREE_TYPE (cfun->decl)))
		   && (int_size_in_bytes (TREE_TYPE (TREE_TYPE (cfun->decl)))
		       > 12))
	    sorry ("return-type larger than 12 bytes with\
 -march=common_v10_v32");
	  else if (current_function_calls_eh_return)
	    {
	      /* The installed EH-return address is in *this* frame, so
		 we need to pop it before we return.  Re-use
		 CRIS_STACKADJ_REG to jump through.  */
	      fprintf (file, "\tmove [$sp+],$srp\n");
	      fprintf (file, "\tadd.d $%s,$sp\n",
		       reg_names[CRIS_STACKADJ_REG]);
	      fprintf (file, "\tmove $srp,$%s\n",
		       reg_names[CRIS_STACKADJ_REG]);
	      fprintf (file, "\tjump $%s\n\tsetf\n",
		       reg_names[CRIS_STACKADJ_REG]);
	    }
	  else
	    fprintf (file, "\tmove.d [$sp+],$r13\n\tjump $r13\n\tsetf\n");
	}
      else
	{
	  if (file == NULL)
	    ;
	  else if (current_function_calls_eh_return)
	    {
	      if (for_sibcall)
		abort ();

	      /* The installed EH-return address is in *this* frame, so we
		 need to pop it before we return.  */
	      fprintf (file, "\tpop $srp\n");
	      fprintf (file, "\tret\n");
	      fprintf (file, "\tadd.d $%s,$sp\n",
		       reg_names[CRIS_STACKADJ_REG]);
	    }
	  else if (for_sibcall)
	    fprintf (file, "\tpop $srp\n");
	  else
	    fprintf (file, "\tJump [$sp+]\n");
	}

      /* Do a sanity check to avoid generating invalid code.  */
      if (file != NULL && current_function_epilogue_delay_list)
	internal_error ("allocated but unused delay list in epilogue");
      return;
    }

  /* Rather than add current_function_calls_eh_return conditions
     everywhere in the following code (and not be able to test it
     thoroughly), assert the assumption that all usage of
     __builtin_eh_return are handled above.  */
  if (current_function_calls_eh_return)
    internal_error ("unexpected function type needing stack adjustment for\
 __builtin_eh_return");

  /* If we pushed some register parameters, then adjust the stack for
     them.  */
  if (pretend)
    {
      if (*save_last && file)
	fprintf (file, save_last);
      *save_last = 0;

      /* Since srp is stored on the way, we need to restore it first.  */
      if (return_address_on_stack)
	{
	  if (file)
	    fprintf (file, "\tmove [$sp+],$srp\n");
	}

      if (pretend <= 63)
	sprintf (save_last, "\taddq %d,$sp\n", pretend);
      else if (file != NULL)
	fprintf (file, "\tadd%s %d,$sp\n",
		 ADDITIVE_SIZE_MODIFIER (pretend), pretend);
    }

  /* Here's where we have a delay-slot we need to fill.  */
  if (file == NULL)
    {
      if (has_delay_slot != NULL && !for_sibcall)
	*has_delay_slot = save_last[0] == 0;
    }
  else if (current_function_epilogue_delay_list && !for_sibcall)
    {
      /* If gcc has allocated an insn for the epilogue delay slot, but
	 things were arranged so we now thought we could do it
	 ourselves, don't forget to flush that insn.  */
      if (*save_last)
	fprintf (file, save_last);

      if (TARGET_V10_V32_COMPATIBLE)
	/* We don't have a delay-slot list in this case.  */
	abort ();

      if (!for_sibcall)
	fprintf (file, "\tRet\n");

      /* Output the delay-slot-insn the mandated way.  */
      final_scan_insn (XEXP (current_function_epilogue_delay_list, 0),
		       file, 1, -2, 1);
    }
  else
    {
      if (TARGET_V10_V32_COMPATIBLE)
	{
	  if (*save_last)
	    fprintf (file, save_last);

	  /* See above for comment on R13 and this test.  */
	  if (!aggregate_value_p (TREE_TYPE (TREE_TYPE (cfun->decl)))
	      && (int_size_in_bytes (TREE_TYPE (TREE_TYPE (cfun->decl)))
		  > 12))
	    sorry ("return-type larger than 12 bytes with -march=common_v10_v32");
	  fprintf (file, "\tmove $srp,$r13\n\tjump $r13\n\tsetf\n");
	}
      else
	{
	  if (!for_sibcall)
	    fprintf (file, "\tRet\n");

	  /* If the GCC did not do it, we have to use whatever insn we
	     have, or a nop.  */
	  if (*save_last)
	    fprintf (file, save_last);
	  else if (!for_sibcall)
	    fprintf (file, "\tnOp\n");
	}
    }
}

/* The TARGET_ASM_FUNCTION_EPILOGUE worker.  */

static void
cris_target_asm_function_epilogue (file, size)
     FILE *file;
     HOST_WIDE_INT size;
{
  cris_target_asm_function_epilogue_1 (file, size, 0, NULL);
}

/* Return non-zero if REG is set in INSN, not counting post-increments.  */

static int
cris_reg_set_in (reg, insn)
     rtx reg;
     rtx insn;
{
  if (INSN_P (insn) || JUMP_P (insn) || GET_CODE (insn) == CALL_INSN)
    return cris_reg_set_in (reg, PATTERN (insn));

  if (GET_CODE (insn) == PARALLEL)
    {
      int i;
      for (i = 0; i < XVECLEN (insn, 0); i++)
	if (GET_CODE (XVECEXP (insn, 0, i)) == SET
	    && (REG_P (XEXP (XVECEXP (insn, 0, i), 0))
		|| (GET_CODE (XEXP (XVECEXP (insn, 0, i), 0))
		    == STRICT_LOW_PART))
	    && cris_reg_overlap_mentioned_p (reg,
					     SET_DEST (XVECEXP (insn, 0, i))))
	  return 1;
      return 0;
    }

  if (GET_CODE (insn) == SET
      && (REG_P (SET_DEST (insn))
	  || GET_CODE (SET_DEST (insn)) == STRICT_LOW_PART)
      && cris_reg_overlap_mentioned_p (reg, SET_DEST (insn)))
    return 1;

  return 0;
}

/* If x is not a MEM, return 0.
   If the register referred to by the MEM is set by INSN, return 1.
   Otherwise return -1.  Suitable for for_each_rtx.  */

static int
cris_mem_reg_set_in (xp, insn)
     rtx *xp;
     void *insn;
{
  rtx reg;
  rtx x = *xp;

  /* This thing is NULL-terminated, which for_each_rtx doesn't handle well.  */
  if (GET_CODE (x) == INSN_LIST)
    return -1;

  if (GET_CODE (x) != MEM)
    return 0;

  reg = XEXP (x, 0);
  if (GET_CODE (reg) == POST_INC)
    reg = XEXP (reg, 0);
  if (!REG_P (reg))
    abort ();
  return cris_reg_set_in (reg, insn);
}

/* If x is a MEM, return 1, else return 0.  Suitable for for_each_rtx.  */

static int
cris_mem_in (xp, foo)
     rtx *xp;
     void *foo ATTRIBUTE_UNUSED;
{
  rtx x = *xp;

  /* This thing is NULL-terminated, which for_each_rtx doesn't handle well.  */
  if (GET_CODE (x) == INSN_LIST)
    return -1;

  if (GET_CODE (x) != MEM)
    return 0;

  return 1;
}

/* TARGET_SCHED_ADJUST_COST worker.  */

static int
cris_adjust_cost (insn, link, dep_insn, cost)
     rtx insn;
     rtx link;
     rtx dep_insn;
     int cost;
{
  /* This is supposed to be the default cost.  */
  if (cost != 1)
    abort ();

  /* We can't check register numbers until after reload.  */
  if (!reload_completed)
    return cost;

  /* FIXME: Check when this happens.  The c4x mentions clobbers.  Does
     it happen for asms too?  */
  if (recog_memoized (dep_insn) < 0)
    return 0;

  if (REG_NOTE_KIND (link) == 0)
    {
      /* Data dependency; DEP_INSN writes a register that INSN reads some
	 cycles later.  */

      enum attr_hazard attrval;
      rtx reg = NULL_RTX;

      if (!TARGET_V32)
	return 1;

      /* When a MOVEM source or destination is involved, it may cause up
	 to three stall cycles.  FIXME: Unfortunately, there's no
	 apparent way to describe that this sequence needs two extra
	 cycles (three cycles being the return value from here):

	  MOVEM [R10],R7
	  MOVE.S R8,R1
	  SUBQ  1,R1
	 while this sequence needs no extra cycles:
	  MOVEM [R10],R7
	  MOVE.S R8,R1
	  SUBQ 1, R8
	i.e. it's kind of an output-depencence that ignores the
	straight overwrite, not simply an output-dependence or
	anti-dependence.  Fortunately, that sequence should be rare,
	since loads happen at the end of the function, with no use of
	those registers before the return.

	Except as above, for movem loads (from memory), the register
	must not be used as a source for the next three instructions, or
	there will be one to three stall cycles.

	The movem load is special; it relates to the dep_insn and
	dominates the other hazards, so let's check it first.  */

      /* Avoid calling get_attr_hazard for all non-parallel (most)
         insns.  */
      if (GET_CODE (PATTERN (dep_insn)) == PARALLEL
	  /* It's highly unlikely (try impossible, at least without
	     inter-function analysis) that we'll get a jump+movem
	     feeding into another insn, but let's describe it for
	     completeness.  */
	  && ((attrval = get_attr_hazard (dep_insn)) == HAZARD_MOVEM_LOAD
	      || attrval == HAZARD_JUMP_MOVEM))
	{
	  int i;
	  rtx x = PATTERN (dep_insn);

	  for (i = (attrval == HAZARD_JUMP_MOVEM ? 2 : 0);
	       i < XVECLEN (x, 0); i++)
	    {
	      reg = SET_DEST (XVECEXP (x, 0, i));
	      if (REG_P (reg))
		{
		  if (GET_CODE (SET_SRC (XVECEXP (x, 0, i))) == MEM)
		    {
		      /* Register (other than mem source reg) set by the
			 movem referenced (that is, used as source) in
			 this insn?  */
		      if (reg_referenced_p (reg, PATTERN (insn)))
			return 4;
		    }
		  else if (i != 1)
		    abort ();
		}
	      else
		abort ();
	    }

	  /* There's still the hope that this we're seeing a dependency
	     (indirectly, through the stack pointer) between a "ret" and
	     a restoring "movem".  If so, we want another insn instead
	     of the movem in the delay-slot, because the calling
	     function is likely to access a saved register as one of the
	     three insns immediately after the return, which will cost
	     stall cycles.  For this case, say that the return costs two
	     cycles wrt. the movem, so --if all goes well-- another insn
	     will fill the ret delay-slot (with yet another insn before
	     it) and there will be three non-movem instructions/cycles
	     at the end of the function.  This unfortunately is not
	     applicable for the common case when we have a unified
	     jump+movem.  Revisit for the DFA scheduler.  */
	  if (attrval == HAZARD_MOVEM_LOAD
	      && GET_CODE (insn) == JUMP_INSN
	      && get_attr_hazard (insn) == HAZARD_RET)
	    return 3;

	  /* If none of that matched; none of the registers (except by
	     post-increment) are set by dep_insn, then none of the
	     hazards below will match either.  */
	  return cost;
	}

      /* For movem stores (to memory), there will be respectively one or
	 two stall cycles (two or three cycles total) if the register
	 was modified in the second last or last instruction.  (Except
	 of course as above, if the modifying insn was a movem load.)
	 The cache imposes two stall cycles if memory is read in one
	 of the two cycles following a write.

	 Most other hazards cause one stall cycle when the modifying insn
	 is not a movem:
	 - Jump to an insn that spans a cache boundary, but we don't
	   model this.
	 - An access spanning a cache boundary, which we also don't
	   model.
	 - Memory access through a register modified in the previous
	   cycle except by autoincrement.  We have one attribute for
	   every location of the mem.
	 - A multiplication with a register modified likewise.
	 - Jump to register modified in the last insn, likewise except
	   by autoincrement, which GCC would anyway never do.  There are
	   several attributes, corresponding to each md use.  */

      attrval = get_attr_hazard (insn);

      switch (attrval)
	{
	case HAZARD_MUL12:
	  /* Neither operand of a multiplication must be modified in the
             preceding cycle.  */
	  extract_insn_cached (insn);
	  if (cris_reg_set_in (recog_data.operand[2], dep_insn))
	    return 2;
	  /* FALL THROUGH.  */

	case HAZARD_CASESI_SRC:
	case HAZARD_CALL_VALUE_SRC:
	  extract_insn_cached (insn);
	  reg = recog_data.operand[1];
	  break;

	  /* Indirect jumps are rare enough that this one didn't trig
	     any change for the whole of gcc libs, glibc, gs-5.50 and
	     ipps-1.40.  */
	case HAZARD_JUMP_SRC:
	case HAZARD_CALL_SRC:
	  extract_insn_cached (insn);
	  reg = recog_data.operand[0];
	  break;

	case HAZARD_RET:
	  reg = gen_rtx_raw_REG (SImode, CRIS_SRP_REGNUM);
	  break;

	case HAZARD_JUMP_MOVEM:
	  /* This insn reads memory, so we need to check if dep_insn
	     sets memory.  The jump takes one cycle, and the insn before
	     that must not set memory.  */
	  if ((GET_CODE (PATTERN (dep_insn)) == SET
	       && GET_CODE (SET_DEST (PATTERN (dep_insn))) == MEM)
	      || (GET_CODE (PATTERN (dep_insn)) == PARALLEL
		  && GET_CODE (XVECEXP (PATTERN (dep_insn), 0, 0)) == SET
		  && (GET_CODE (SET_DEST (XVECEXP (PATTERN (dep_insn), 0, 0)))
		      == MEM)))
	    return 2;

	  /* The mem source address of the movem will never cause a
	     hazard, because the jump is right before it and it doesn't
	     cause a stall.  We just need to check whether the
	     return-address register is set by dep_insn.  */
	  extract_insn_cached (insn);
	  reg = recog_data.operand[1];
	  break;

	case HAZARD_MOVEM_STORE:
	  {
	    int i;
	    rtx x = PATTERN (insn);

	    /* The stored registers must be unmodified by the two
	       preceding insns (or three, if modified by movem).  */
	    for (i = 0; i < XVECLEN (x, 0); i++)
	      {
		reg = SET_SRC (XVECEXP (x, 0, i));
		if (REG_P (reg))
		  {
		    if (cris_reg_set_in (reg, dep_insn))
		      return 3;
		  }
		else if (i != 1)
		  abort ();
	      }

	    /* To check the address.  */
	    reg = XEXP (SET_DEST (XVECEXP (x, 0, 0)), 0);
	    break;
	  }

	case HAZARD_MOVEM_LOAD:
	  /* If dep_insn sets a mem, this causes two stall cycles.  */
	  if ((GET_CODE (PATTERN (dep_insn)) == SET
	       && GET_CODE (SET_DEST (PATTERN (dep_insn))) == MEM)
	      || (GET_CODE (PATTERN (dep_insn)) == PARALLEL
		  && GET_CODE (XVECEXP (PATTERN (dep_insn), 0, 0)) == SET
		  && (GET_CODE (SET_DEST (XVECEXP (PATTERN (dep_insn), 0, 0)))
		      == MEM)))
	    return 3;

	  /* To check the address.  */
	  reg = XEXP (SET_SRC (XVECEXP (PATTERN (insn), 0, 0)), 0);
	  break;

	case HAZARD_NORMAL:
	  /* Check for memory operands.  First, if dep_insn sets a mem
	     and insn reads one, this causes two stall cycles.  */
	  if (((GET_CODE (PATTERN (dep_insn)) == SET
		&& GET_CODE (SET_DEST (PATTERN (dep_insn))) == MEM)
	       || (GET_CODE (PATTERN (dep_insn)) == PARALLEL
		   && GET_CODE (XVECEXP (PATTERN (dep_insn), 0, 0)) == SET
		   && (GET_CODE (SET_DEST (XVECEXP (PATTERN (dep_insn), 0, 0)))
		       == MEM)))
	      && for_each_rtx (GET_CODE (PATTERN (insn)) == SET
				 ? &SET_SRC (PATTERN (insn))
				 : &PATTERN (insn), cris_mem_in, NULL) != 0)
	    return 3;

	  /* Then, if dep_insn sets a register used in insn as an
	     address.  */
	  if (for_each_rtx (&PATTERN (insn), cris_mem_reg_set_in, dep_insn))
	    return 2;
	  return cost;

	  /* Just marker insns (for marking stack as deallocated) with
	     no real insn.  */
	case HAZARD_FAKED_INSN:
	  return 0;

	default:
	  return cost;
	}

      /* Insns with a memory access (or dependency with similar
	 behavior) come here with "reg" set to a register or POST_INC of
	 a register, checking whether it's set in dep_insn.  */
      if (cris_reg_set_in (reg, dep_insn))
	return 2;

      return cost;
    }
  else if (REG_NOTE_KIND (link) == REG_DEP_ANTI
	   || REG_NOTE_KIND (link) == REG_DEP_OUTPUT)
      /* Anti dependency: DEP_INSN reads a register that INSN writes
	 some cycles later.  Output dependency: DEP_INSN writes a
	 register that INSN writes some cycles later.  */
    return 0;
  else
    abort ();
}

/* The PRINT_OPERAND worker.  */

void
cris_print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  rtx operand = x;

  /* Size-strings corresponding to MULT expressions.  */
  static const char *mults[] = { "BAD:0", ".b", ".w", "BAD:3", ".d" };

  /* New code entries should just be added to the switch below.  If
     handling is finished, just return.  If handling was just a
     modification of the operand, the modified operand should be put in
     "operand", and then do a break to let default handling
     (zero-modifier) output the operand.  */

  switch (code)
    {
    case 'b':
      /* Print the unsigned supplied integer as if it was signed
	 and < 0, i.e print 255 or 65535 as -1, 254, 65534 as -2, etc.  */
      if (GET_CODE (x) != CONST_INT
	  || ! CONST_OK_FOR_LETTER_P (INTVAL (x), 'O'))
	LOSE_AND_RETURN ("invalid operand for 'b' modifier", x);
      fprintf (file, "%d", INTVAL (x)| (INTVAL (x) <= 255 ? ~255 : ~65535));
      return;

    case 'x':
      /* Print assembler code for operator.  */
      fprintf (file, "%s", cris_op_str (operand));
      return;

    case 'o':
      {
	/* A movem modifier working on a parallel; output the register
	   name.  */
	int regno;
	int offs = 0;

	if (GET_CODE (x) != PARALLEL)
	  LOSE_AND_RETURN ("invalid operand for 'o' modifier", x);

	/* Might be a [(return) (use reg) movem-stuff].  */
	if (GET_CODE (XVECEXP (x, 0, 0)) == RETURN)
	  {
	    if (GET_CODE (XVECEXP (x, 0, 1)) != USE
		|| !REG_P (XEXP (XVECEXP (x, 0, 1), 0)))
	      abort ();
	    offs = 2;
	  }

	/* The second item can be (set reg (plus reg const)) to denote a
	   postincrement.  */
	regno
	  = (GET_CODE (SET_SRC (XVECEXP (x, 0, offs + 1))) == PLUS
	     ? XVECLEN (x, 0) - 2 - offs
	     : XVECLEN (x, 0) - 1 - offs);

	fprintf (file, "$%s", reg_names [regno]);
      }
      return;

    case 'O':
      {
	/* A similar movem modifier; output the memory operand.  */
	rtx addr;

	int offs = 0;

	if (GET_CODE (x) != PARALLEL)
	  LOSE_AND_RETURN ("invalid operand for 'O' modifier", x);

	/* Might be a [(return) (use reg) movem-stuff].  */
	if (GET_CODE (XVECEXP (x, 0, 0)) == RETURN)
	  {
	    if (GET_CODE (XVECEXP (x, 0, 1)) != USE
		|| !REG_P (XEXP (XVECEXP (x, 0, 1), 0)))
	      abort ();
	    offs = 2;
	  }

	/* The lowest mem operand is in the first item, but perhaps it
	   needs to be output as postincremented.  */
	addr = GET_CODE (SET_SRC (XVECEXP (x, 0, offs))) == MEM
	  ? XEXP (SET_SRC (XVECEXP (x, 0, offs)), 0)
	  : XEXP (SET_DEST (XVECEXP (x, 0, offs)), 0);

	/* The second item can be a (set reg (plus reg const)) to denote a
	   post-increment.  */
	if (GET_CODE (SET_SRC (XVECEXP (x, 0, offs + 1))) == PLUS)
	  addr = gen_rtx_POST_INC (SImode, addr);

	output_address (addr);
      }
      return;

    case 'p':
      /* Adjust a power of two to its log2.  */
      if (GET_CODE (x) != CONST_INT || exact_log2 (INTVAL (x)) < 0 )
	LOSE_AND_RETURN ("invalid operand for 'p' modifier", x);
      fprintf (file, "%d", exact_log2 (INTVAL (x)));
      return;

    case 's':
      /* For an integer, print 'b' or 'w' if <= 255 or <= 65535
	 respectively.  This modifier also terminates the inhibiting
         effects of the 'x' modifier.  */
      cris_output_insn_is_bound = 0;
      if (GET_MODE (x) == VOIDmode && GET_CODE (x) == CONST_INT)
	{
	  if (INTVAL (x) >= 0)
	    {
	      if (INTVAL (x) <= 255)
		putc ('b', file);
	      else if (INTVAL (x) <= 65535)
		putc ('w', file);
	      else
		putc ('d', file);
	    }
	  else
	    putc ('d', file);
	  return;
	}

      /* For a non-integer, print the size of the operand.  */
      putc ((GET_MODE (x) == SImode || GET_MODE (x) == SFmode)
	    ? 'd' : GET_MODE (x) == HImode ? 'w'
	    : GET_MODE (x) == QImode ? 'b'
	    /* If none of the above, emit an erroneous size letter.  */
	    : 'X',
	    file);
      return;

    case 'z':
      /* Const_int: print b for -127 <= x <= 255,
	 w for -32768 <= x <= 65535, else abort.  */
      if (GET_CODE (x) != CONST_INT
	  || INTVAL (x) < -32768 || INTVAL (x) > 65535)
	LOSE_AND_RETURN ("invalid operand for 'z' modifier", x);
      putc (INTVAL (x) >= -128 && INTVAL (x) <= 255 ? 'b' : 'w', file);
      return;

    case 'Z':
      /* If this is a GOT-symbol, print the size-letter corresponding to
	 -fpic/-fPIC.  For everything else, print "d".  */
      putc ((flag_pic == 1
	     && GET_CODE (x) == CONST
	     && GET_CODE (XEXP (x, 0)) == UNSPEC
	     && XINT (XEXP (x, 0), 1) == CRIS_UNSPEC_GOTREAD)
	    ? 'w' : 'd', file);
      return;
	
    case '#':
      /* Output a 'nop' if there's nothing for the delay slot.
	 This method stolen from the sparc files.  */
      if (dbr_sequence_length () == 0)
	{
	  if (TARGET_V10_V32_COMPATIBLE)
	    fputs ("\n\tsetf", file);
	  else
	    fputs ("\n\tnop", file);
	}
      return;

    case '!':
      /* Output directive for alignment padded with "nop" insns.
	 Optimizing for size, it's plain 4-byte alignment, otherwise we
	 align the section to a cache-line (32 bytes) and skip at max 2
	 bytes, i.e. we skip if it's the last insn on a cache-line.  The
	 latter is faster by a small amount (for two test-programs 99.6%
	 and 99.9%) and larger by a small amount (ditto 100.1% and
	 100.2%).  This is supposed to be the simplest yet performance-
	 wise least intrusive way to make sure the immediately following
	 (supposed) muls/mulu insn isn't located at the end of a
	 cache-line.  */
      if (TARGET_MUL_BUG)
	fputs (optimize_size
	       ? ".p2alignw 2,0x050f\n\t"
	       : ".p2alignw 5,0x050f,2\n\t", file);
      return;

    case ':':
      /* The PIC register.  */
      if (! flag_pic)
	internal_error ("invalid use of ':' modifier");
      fprintf (file, "$%s", reg_names [PIC_OFFSET_TABLE_REGNUM]);
      return;

    case 'H':
      /* Print high (most significant) part of something.  */
      switch (GET_CODE (operand))
	{
	case CONST_INT:
	  /* If we're having 64-bit HOST_WIDE_INTs, the whole (DImode)
	     value is kept here, and so may be other than 0 or -1.  */
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC,
		   INTVAL (operand_subword (operand, 1, 0, DImode)));
	  return;

	case CONST_DOUBLE:
	  /* High part of a long long constant.  */
	  if (GET_MODE (operand) == VOIDmode)
	    {
	      fprintf (file, "0x%x", CONST_DOUBLE_HIGH (x));
	      return;
	    }
	  else
	    LOSE_AND_RETURN ("invalid operand for 'H' modifier", x);

	case REG:
	  /* Print reg + 1.  Check that there's not an attempt to print
	     high-parts of registers like stack-pointer or higher.  */
	  if (REGNO (operand) > STACK_POINTER_REGNUM - 2)
	    LOSE_AND_RETURN ("bad register", operand);
	  fprintf (file, "$%s", reg_names[REGNO (operand) + 1]);
	  return;

	case MEM:
	  /* Adjust memory address to high part.  */
	  {
	    rtx adj_mem = operand;
	    int size
	      = GET_MODE_BITSIZE (GET_MODE (operand)) / BITS_PER_UNIT;

	    /* Adjust so we can use two SImode in DImode.
	       Calling adj_offsettable_operand will make sure it is an
	       offsettable address.  Don't do this for a postincrement
	       though; it should remain as it was.  */
	    if (GET_CODE (XEXP (adj_mem, 0)) != POST_INC)
	      adj_mem
		= adjust_address (adj_mem, GET_MODE (adj_mem), size / 2);

	    output_address (XEXP (adj_mem, 0));
	    return;
	  }

	default:
	  LOSE_AND_RETURN ("invalid operand for 'H' modifier", x);
	}

    case 'L':
      /* Strip the MEM expression.  */
      operand = XEXP (operand, 0);

      /* If the inside is a POST_INC, we're supposed to strip that too.  */
      if (GET_CODE (operand) == POST_INC)
	operand = XEXP (operand, 0);
      break;

    case 'e':
      /* Print 's' if operand is SIGN_EXTEND or 'u' if ZERO_EXTEND unless
	 cris_output_insn_is_bound is nonzero.  */
      if (GET_CODE (operand) != SIGN_EXTEND
	  && GET_CODE (operand) != ZERO_EXTEND
	  && GET_CODE (operand) != CONST_INT)
	LOSE_AND_RETURN ("invalid operand for 'e' modifier", x);

      if (cris_output_insn_is_bound)
	{
	  cris_output_insn_is_bound = 0;
	  return;
	}

      putc (GET_CODE (operand) == SIGN_EXTEND
	    || (GET_CODE (operand) == CONST_INT && INTVAL (operand) < 0)
	    ? 's' : 'u', file);
      return;

    case 'm':
      /* Print the size letter of the inner element.  We can do it by
	 calling ourselves with the 's' modifier.  */
      if (GET_CODE (operand) != SIGN_EXTEND && GET_CODE (operand) != ZERO_EXTEND)
	LOSE_AND_RETURN ("invalid operand for 'm' modifier", x);
      cris_print_operand (file, XEXP (operand, 0), 's');
      return;

    case 'M':
      /* Print the least significant part of operand.  */
      if (GET_CODE (operand) == CONST_DOUBLE)
	{
	  fprintf (file, "0x%x", CONST_DOUBLE_LOW (x));
	  return;
	}
      else if (HOST_BITS_PER_WIDE_INT > 32 && GET_CODE (operand) == CONST_INT)
	{
	  fprintf (file, "0x%x",
		   INTVAL (x) & ((unsigned int) 0x7fffffff * 2 + 1));
	  return;
	}
      /* Otherwise the least significant part equals the normal part,
	 so handle it normally.  */
      break;

    case 'A':
      /* When emitting an add for the high part of a DImode constant, we
	 want to use addq for 0 and adds.w for -1.  */
      if (GET_CODE (operand) != CONST_INT)
	LOSE_AND_RETURN ("invalid operand for 'A' modifier", x);
      fprintf (file, INTVAL (operand) < 0 ? "adds.w" : "addq");
      return;

    case 'd':
      /* If this is a GOT symbol, force it to be emitted as :GOT and
	 :GOTPLT regardless of -fpic (i.e. not as :GOT16, :GOTPLT16).
	 Avoid making this too much of a special case.  */
      if (flag_pic == 1 && CONSTANT_P (operand))
	{
	  int flag_pic_save = flag_pic;

	  flag_pic = 2;
	  cris_output_addr_const (file, operand);
	  flag_pic = flag_pic_save;
	  return;
	}
      break;

    case 'D':
      /* When emitting an sub for the high part of a DImode constant, we
	 want to use subq for 0 and subs.w for -1.  */
      if (GET_CODE (operand) != CONST_INT)
	LOSE_AND_RETURN ("invalid operand for 'D' modifier", x);
      fprintf (file, INTVAL (operand) < 0 ? "subs.w" : "subq");
      return;

    case 'S':
      /* Print the operand as the index-part of an address.
	 Easiest way out is to use cris_print_index.  */
      cris_print_index (operand, file);
      return;

    case 'T':
      /* Print the size letter for an operand to a MULT, which must be a
	 const_int with a suitable value.  */
      if (GET_CODE (operand) != CONST_INT || INTVAL (operand) > 4)
	LOSE_AND_RETURN ("invalid operand for 'T' modifier", x);
      fprintf (file, "%s", mults[INTVAL (operand)]);
      return;

    case 'u':
      /* Print "u.w" if a GOT symbol and flag_pic == 1, else ".d".  */
      if (flag_pic == 1
	  && GET_CODE (operand) == CONST
	  && GET_CODE (XEXP (operand, 0)) == UNSPEC
	  && XINT (XEXP (operand, 0), 1) == CRIS_UNSPEC_GOTREAD)
	fprintf (file, "u.w");
      else
	fprintf (file, ".d");
      return;

    case 0:
      /* No code, print as usual.  */
      break;

    default:
      LOSE_AND_RETURN ("invalid operand modifier letter", x);
    }

  /* Print an operand as without a modifier letter.  */
  switch (GET_CODE (operand))
    {
    case REG:
      if (REGNO (operand) > 15
	  && REGNO (operand) != CRIS_MOF_REGNUM
	  && REGNO (operand) != CRIS_SRP_REGNUM)
	internal_error ("internal error: bad register: %d", REGNO (operand));
      fprintf (file, "$%s", reg_names[REGNO (operand)]);
      return;

    case MEM:
      output_address (XEXP (operand, 0));
      return;

    case CONST_DOUBLE:
      if (GET_MODE (operand) == VOIDmode)
	/* A long long constant.  */
	output_addr_const (file, operand);
      else
	{
	  /* Only single precision is allowed as plain operands the
	     moment.  FIXME:  REAL_VALUE_FROM_CONST_DOUBLE isn't
	     documented.  */
	  REAL_VALUE_TYPE r;
	  long l;

	  /* FIXME:  Perhaps check overflow of the "single".  */
	  REAL_VALUE_FROM_CONST_DOUBLE (r, operand);
	  REAL_VALUE_TO_TARGET_SINGLE (r, l);

	  fprintf (file, "0x%lx", l);
	}
      return;

    case UNSPEC:
      /* Fall through.  */
    case CONST:
      cris_output_addr_const (file, operand);
      return;

    case MULT:
    case ASHIFT:
      {
	/* For a (MULT (reg X) const_int) we output "rX.S".  */
	int i = GET_CODE (XEXP (operand, 1)) == CONST_INT
	  ? INTVAL (XEXP (operand, 1)) : INTVAL (XEXP (operand, 0));
	rtx reg = GET_CODE (XEXP (operand, 1)) == CONST_INT
	  ? XEXP (operand, 0) : XEXP (operand, 1);

	if (GET_CODE (reg) != REG
	    || (GET_CODE (XEXP (operand, 0)) != CONST_INT
		&& GET_CODE (XEXP (operand, 1)) != CONST_INT))
	  LOSE_AND_RETURN ("unexpected multiplicative operand", x);

	cris_print_base (reg, file);
	fprintf (file, ".%c",
		 i == 0 || (i == 1 && GET_CODE (operand) == MULT) ? 'b'
		 : i == 4 ? 'd'
		 : (i == 2 && GET_CODE (operand) == MULT) || i == 1 ? 'w'
		 : 'd');
	return;
      }

    default:
      /* No need to handle all strange variants, let output_addr_const
	 do it for us.  */
      if (CONSTANT_P (operand))
	{
	  cris_output_addr_const (file, operand);
	  return;
	}

      LOSE_AND_RETURN ("unexpected operand", x);
    }
}

/* The PRINT_OPERAND_ADDRESS worker.  */

void
cris_print_operand_address (file, x)
     FILE *file;
     rtx x;
{
  /* All these were inside MEM:s so output indirection characters.  */
  putc ('[', file);

  if (CONSTANT_ADDRESS_P (x))
    cris_output_addr_const (file, x);
  else if (BASE_OR_AUTOINCR_P (x))
    cris_print_base (x, file);
  else if (GET_CODE (x) == PLUS)
    {
      rtx x1, x2;

      x1 = XEXP (x, 0);
      x2 = XEXP (x, 1);
      if (BASE_P (x1))
	{
	  cris_print_base (x1, file);
	  cris_print_index (x2, file);
	}
      else if (BASE_P (x2))
	{
	  cris_print_base (x2, file);
	  cris_print_index (x1, file);
	}
      else
	LOSE_AND_RETURN ("unrecognized address", x);
    }
  else if (GET_CODE (x) == MEM)
    {
      /* A DIP.  Output more indirection characters.  */
      putc ('[', file);
      cris_print_base (XEXP (x, 0), file);
      putc (']', file);
    }
  else
    LOSE_AND_RETURN ("unrecognized address", x);

  putc (']', file);
}

/* Calculate the frame layout.  For now, we just calculate whether we
   need the return address explicitly stored on the stack, which happens
   when we're out of free registers for V32, and all the time for pre-V32.  */

static void
cris_calculate_frame_layout (recalc)
     int recalc;
{
  /* Let's make it multi-call safe so the caller doesn't have to check.  */
  if (cfun->machine->frame_layout_calculated  && !recalc)
    return;

  /* FIXME: Maybe adjust when we emit prologue and epilogue for pre-V32
     as RTL.  */
  if (!TARGET_V32)
    {
      if (regs_ever_live[CRIS_SRP_REGNUM]
	  || cfun->machine->needs_return_address_on_stack)
	cfun->machine->has_return_address_on_stack = 1;
    }
  else if (regs_ever_live[CRIS_SRP_REGNUM])
    {
      /* Find out whether we have an available call-saved register or we
	 need to put the return address on stack.  */
      unsigned int regno;

      cfun->machine->srp_save_register = NULL_RTX;
      cfun->machine->has_return_address_on_stack = 0;

      if (cfun->machine->needs_return_address_on_stack
	  || current_function_calls_eh_return)
	/* Usually they call __builtin_return_address which causes
	   cfun->machine->needs_return_address_on_stack to be set, but
	   we shouldn't depend on that.  To avoid forcing to stack, we'd
	   need to use a special pseudo for this, because the eh_return
	   code is generated before we know where the register ends up.
	   (It's really of no practical use to try that.)  */
	cfun->machine->has_return_address_on_stack = 1;
      else
	{
	  for (regno = 0;
	       /* Assumption: the call-saved registers (those not "fixed")
		  are contiguous and not overlapping parameter registers and
		  such, unless also noted in regs_ever_live[].  */
	       (!fixed_regs[regno]
		&& regs_ever_live[regno] != 0
		&& call_used_regs[regno] == 0
		&& (regno != FRAME_POINTER_REGNUM || !frame_pointer_needed));
	       regno++)
	    {
	      if (regno > CRIS_LAST_GENERAL_REGISTER)
		abort ();
	    }

	  if (regs_ever_live[regno] == 0
	      && call_used_regs[regno] == 0
	      /* We'd get a longer sequence if we'd movem because of
		 the return-address register only.  */
	      && regno != 0)
	    cfun->machine->srp_save_register = gen_rtx_raw_REG (Pmode, regno);
	  else
	    cfun->machine->has_return_address_on_stack = 1;
	}
    }

  cfun->machine->frame_layout_calculated = 1;
}

/* The RETURN_ADDR_RTX worker.
   We mark that the return address is used, either by EH or
   __builtin_return_address, for use by the function prologue and
   epilogue.  FIXME: This isn't optimal; we just use the mark in the
   prologue and epilogue to say that the return address is to be stored
   in the stack frame.  We could return SRP for leaf-functions and use the
   initial-value machinery.  */

rtx
cris_return_addr_rtx (count, frameaddr)
     int count;
     rtx frameaddr ATTRIBUTE_UNUSED;
{
  /* For the small number of library functions that call
     __builtin_eh_return (), we stick with the return address in the
     known slot.  For others, make it an initial-value.  FIXME: If
     important enough, we could look into redirecting these
     initial-values to register numbers and stack slots.  */
  if (TARGET_V32 && !current_function_calls_eh_return)
    return count == 0
      ? get_hard_reg_initial_val (Pmode, CRIS_SRP_REGNUM)
      : NULL_RTX;

  cfun->machine->needs_return_address_on_stack = 1;

  /* The return-address is stored just above the saved frame-pointer (if
     present).  Apparently we can't eliminate from the frame-pointer in
     that direction, so use the incoming args (maybe pretended) pointer.  */
  return count == 0
    ? gen_rtx_MEM (Pmode, plus_constant (virtual_incoming_args_rtx, -4))
    : NULL_RTX;
}

/* Accessor used in cris.md:return because cfun->machine isn't available
   there.  */

int
cris_return_address_on_stack ()
{
  return cfun->machine->needs_return_address_on_stack;
}

/* This used to be the INITIAL_FRAME_POINTER_OFFSET worker; now only
   handles FP -> SP elimination offset.  */

static int
cris_initial_frame_pointer_offset ()
{
  int got_really_used = cris_got_really_used ();
  int regno;
  int n_movem_regs = 0;


  /* Initial offset is 0 if we don't have a frame pointer.  */
  int offs = 0;

  /* And 4 for each register pushed.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (cris_reg_saved_in_regsave_area (regno, got_really_used))
      {
	n_movem_regs++;
	offs += 4;
      }

  /* For V32, we save *all* call-saved regs with a movem, so fill in any
     gaps in the sequence from zero up to the last call-saved register.
     If current_function_calls_eh_return, we have some weird clobbering
     of registers, so we then avoid optimizing.  We also don't movem a
     single register.  */
  if (TARGET_V32
      && !current_function_calls_eh_return
      && n_movem_regs != 1)
    {
      for (regno = CRIS_LAST_GENERAL_REGISTER; regno >= 0; regno--)
	if (cris_reg_saved_in_regsave_area (regno, got_really_used))
	  break;

      if (regno != -1)
	{
	  int i;
	  for (i = 0; i < regno; i++)
	    if (!cris_reg_saved_in_regsave_area (i, got_really_used))
	      offs += 4;
	}
    }

  /* And then, last, we add the locals allocated.  */
  offs += get_frame_size ();

  /* And more; the accumulated args size.  */
  offs += current_function_outgoing_args_size;

  /* Then round it off, in case we use aligned stack.  */
  if (TARGET_STACK_ALIGN)
    offs = TARGET_ALIGN_BY_32 ? (offs + 3) & ~3 : (offs + 1) & ~1;

  return offs;
}

/* The INITIAL_ELIMINATION_OFFSET worker.
   Calculate the difference between imaginary registers such as frame
   pointer and the stack pointer.  Used to eliminate the frame pointer
   and imaginary arg pointer.  */

int
cris_initial_elimination_offset (fromreg, toreg)
     int fromreg;
     int toreg;
{
  int fp_sp_offset;
  int return_address_on_stack;
  int ap_fp_offset;

  cris_calculate_frame_layout (1);

  fp_sp_offset = cris_initial_frame_pointer_offset ();

  /* We should be able to use regs_ever_live and related prologue
     information here, or alpha should not as well.  */
  return_address_on_stack
    = (!TARGET_V32 && regs_ever_live[CRIS_SRP_REGNUM])
    || cfun->machine->has_return_address_on_stack != 0;

  /* Here we act as if the frame-pointer is needed.  */
  ap_fp_offset = 4 + (return_address_on_stack ? 4 : 0);

  if (fromreg == ARG_POINTER_REGNUM
      && toreg == FRAME_POINTER_REGNUM)
    return ap_fp_offset;

  /* Between the frame pointer and the stack are only "normal" stack
     variables and saved registers.  */
  if (fromreg == FRAME_POINTER_REGNUM
      && toreg == STACK_POINTER_REGNUM)
    return fp_sp_offset;

  /* We need to balance out the frame pointer here.  */
  if (fromreg == ARG_POINTER_REGNUM
      && toreg == STACK_POINTER_REGNUM)
    return ap_fp_offset + fp_sp_offset - 4;

  abort ();
}

/* Worker for cris_notice_update_cc; handles the "normal" cases.  This
   code is historical; its functionality should be refactored to look at
   insn attributes and moved to cris_notice_update_cc.  */
static void
cris_normal_notice_update_cc (exp)
     rtx exp;
{
  /* FIXME: Currently avoid reindentation. */
  {
    {
      /* "Normal" means, for:
	 (set (cc0) (...)):
	 CC is (...).

	 (set (reg) (...)):
	 CC is (reg) and (...) - unless (...) is 0 or reg is a special
	       register or (v32 and (...) is -32..-1), then CC does not change.
	 CC_NO_OVERFLOW unless (...) is reg or mem.

	 (set (mem) (...)):
	 CC does not change.

	 (set (pc) (...)):
	 CC does not change.

	 (parallel
	  (set (reg1) (mem (bdap/biap)))
	  (set (reg2) (bdap/biap))):
	 CC is (reg1) and (mem (reg2))

	 (parallel
	  (set (mem (bdap/biap)) (reg1)) [or 0]
	  (set (reg2) (bdap/biap))):
	 CC does not change.

	 (where reg and mem includes strict_low_parts variants thereof)

	 For all others, assume CC is clobbered.
	 Note that we do not have to care about setting CC_NO_OVERFLOW,
	 since the overflow flag is set to 0 (i.e. right) for
	 instructions where it does not have any sane sense, but where
	 other flags have meanings.  (This includes shifts; the carry is
	 not set by them).

	 Note that there are other parallel constructs we could match,
	 but we don't do that yet.  */

      if (GET_CODE (exp) == SET)
	{
	  /* FIXME: Check when this happens.  It looks like we should
	     actually do a CC_STATUS_INIT here to be safe.  */
	  if (SET_DEST (exp) == pc_rtx)
	    return;

	  /* Record CC0 changes, so we do not have to output multiple
	     test insns.  */
	  if (SET_DEST (exp) == cc0_rtx)
	    {
	      CC_STATUS_INIT;
	      cc_status.value1 = SET_SRC (exp);

	      /* Handle flags for the special btstq on one bit.  */
	      if (GET_CODE (SET_SRC (exp)) == ZERO_EXTRACT
		  && XEXP (SET_SRC (exp), 1) == const1_rtx)
		{
		  if (GET_CODE (XEXP (SET_SRC (exp), 0)) == CONST_INT)
		    /* Using cmpq.  */
		    cc_status.flags = CC_INVERTED;
		  else
		    /* A one-bit btstq.  */
		    cc_status.flags = CC_Z_IN_NOT_N;
		}

	      if (GET_CODE (SET_SRC (exp)) == COMPARE)
		{
		  if (!REG_P (XEXP (SET_SRC (exp), 0))
		      && XEXP (SET_SRC (exp), 1) != const0_rtx)
		    /* For some reason gcc will not canonicalize compare
		       operations, reversing the sign by itself if
		       operands are in wrong order.  */
		    /* (But NOT inverted; eq is still eq.) */
		    cc_status.flags = CC_REVERSED;

		  /* This seems to be overlooked by gcc.  FIXME: Check again.
		     FIXME:  Is it really safe?  */
		  cc_status.value2
		    = gen_rtx_MINUS (GET_MODE (SET_SRC (exp)),
				     XEXP (SET_SRC (exp), 0),
				     XEXP (SET_SRC (exp), 1));
		}
	      return;
	    }
	  else if (REG_P (SET_DEST (exp))
		   || (GET_CODE (SET_DEST (exp)) == STRICT_LOW_PART
		       && REG_P (XEXP (SET_DEST (exp), 0))))
	    {
	      /* A register is set; normally CC is set to show that no
		 test insn is needed.  Catch the exceptions.  */

	      /* If not to cc0, then no "set"s in non-natural mode give
		 ok cc0...  */
	      if (GET_MODE_SIZE (GET_MODE (SET_DEST (exp))) > UNITS_PER_WORD
		  || GET_MODE_CLASS (GET_MODE (SET_DEST (exp))) == MODE_FLOAT)
		{
		  /* ... except add:s and sub:s in DImode.  */
		  if (GET_MODE (SET_DEST (exp)) == DImode
		      && (GET_CODE (SET_SRC (exp)) == PLUS
			  || GET_CODE (SET_SRC (exp)) == MINUS))
		    {
		      CC_STATUS_INIT;
		      cc_status.value1 = SET_DEST (exp);
		      cc_status.value2 = SET_SRC (exp);

		      if (cris_reg_overlap_mentioned_p (cc_status.value1,
							cc_status.value2))
			cc_status.value2 = 0;

		      /* Add and sub may set V, which gets us
			 unoptimizable results in "gt" and "le" condition
			 codes.  */
		      cc_status.flags |= CC_NO_OVERFLOW;

		      return;
		    }
		}
	      else if (SET_SRC (exp) == const0_rtx
		       || (REG_P (SET_SRC (exp))
			   && (REGNO (SET_SRC (exp))
			       > CRIS_LAST_GENERAL_REGISTER))
		       || (TARGET_V32
			   && GET_CODE (SET_SRC (exp)) == CONST_INT
			   && CONST_OK_FOR_LETTER_P (INTVAL (SET_SRC (exp)),
						     'I')))
		{
		  /* There's no CC0 change for this case.  Just check
                     for overlap.  */
		  if ((cc_status.value1
		       && cris_reg_overlap_mentioned_p (SET_DEST (exp),
							cc_status.value1)))
		    cc_status.value1 = 0;

		  if ((cc_status.value2
		       && cris_reg_overlap_mentioned_p (SET_DEST (exp),
							cc_status.value2)))
		    cc_status.value2 = 0;

		  return;
		}
	      else
		{
		  CC_STATUS_INIT;
		  cc_status.value1 = SET_DEST (exp);
		  cc_status.value2 = SET_SRC (exp);

		  if (cris_reg_overlap_mentioned_p (cc_status.value1,
						    cc_status.value2))
		    cc_status.value2 = 0;

		  /* Some operations may set V, which gets us
		     unoptimizable results in "gt" and "le" condition
		     codes.  */
		  if (GET_CODE (SET_SRC (exp)) == PLUS
		      || GET_CODE (SET_SRC (exp)) == MINUS
		      || GET_CODE (SET_SRC (exp)) == NEG)
		    cc_status.flags |= CC_NO_OVERFLOW;

		  /* For V32, nothing with a register destination sets
		     C and V usefully.  */
		  if (TARGET_V32)
		    cc_status.flags |= CC_NO_OVERFLOW;

		  return;
		}
	    }
	  else if (GET_CODE (SET_DEST (exp)) == MEM
		   || (GET_CODE (SET_DEST (exp)) == STRICT_LOW_PART
		       && GET_CODE (XEXP (SET_DEST (exp), 0)) == MEM))
	    {
	      /* When SET to MEM, then CC is not changed (except for
		 overlap).  */
	      if ((cc_status.value1
		   && cris_reg_overlap_mentioned_p (SET_DEST (exp),
						    cc_status.value1)))
		cc_status.value1 = 0;

	      if ((cc_status.value2
		   && cris_reg_overlap_mentioned_p (SET_DEST (exp),
						    cc_status.value2)))
		cc_status.value2 = 0;

	      return;
	    }
	}
      else if (GET_CODE (exp) == PARALLEL)
	{
	  if (GET_CODE (XVECEXP (exp, 0, 0)) == SET
	      && GET_CODE (XVECEXP (exp, 0, 1)) == SET
	      && REG_P (XEXP (XVECEXP (exp, 0, 1), 0)))
	    {
	      if (REG_P (XEXP (XVECEXP (exp, 0, 0), 0))
		  && GET_CODE (XEXP (XVECEXP (exp, 0, 0), 1)) == MEM)
		{
		  CC_STATUS_INIT;

		  /* For "move.S [rx=ry+o],rz", say CC reflects
		     value1=rz and value2=[rx] */
		  cc_status.value1 = XEXP (XVECEXP (exp, 0, 0), 0);
		  cc_status.value2
		    = replace_equiv_address (XEXP (XVECEXP (exp, 0, 0), 1),
					     XEXP (XVECEXP (exp, 0, 1), 0));

		  /* Huh?  A side-effect cannot change the destination
		     register.  */
		  if (cris_reg_overlap_mentioned_p (cc_status.value1,
						    cc_status.value2))
		    internal_error ("internal error: sideeffect-insn affecting main effect");

		  /* For V32, moves to registers don't set C and V.  */
		  if (TARGET_V32)
		    cc_status.flags |= CC_NO_OVERFLOW;
		  return;
		}
	      else if ((REG_P (XEXP (XVECEXP (exp, 0, 0), 1))
			|| XEXP (XVECEXP (exp, 0, 0), 1) == const0_rtx)
		       && GET_CODE (XEXP (XVECEXP (exp, 0, 0), 0)) == MEM)
		{
		  /* For "move.S rz,[rx=ry+o]" and "clear.S [rx=ry+o]",
		     say flags are not changed, except for overlap.  */
		  if (cc_status.value1
		      && cris_reg_overlap_mentioned_p (XEXP
						       (XVECEXP
							(exp, 0, 0), 0),
						       cc_status.value1))
		    cc_status.value1 = 0;

		  if (cc_status.value1
		      && cris_reg_overlap_mentioned_p (XEXP
						       (XVECEXP
							(exp, 0, 1), 0),
						       cc_status.value1))
		    cc_status.value1 = 0;

		  if (cc_status.value2
		      && cris_reg_overlap_mentioned_p (XEXP
						       (XVECEXP
							(exp, 0, 0), 0),
						       cc_status.value2))
		    cc_status.value2 = 0;

		  if (cc_status.value2
		      && cris_reg_overlap_mentioned_p (XEXP
						       (XVECEXP
							(exp, 0, 1), 0),
						       cc_status.value2))
		    cc_status.value2 = 0;

		  return;
		}
	    }
	}
    }
  }

  /* If we got here, the case wasn't covered by the code above, so be
     safe.  */
  CC_STATUS_INIT;
}

/*  This function looks into the pattern to see how this insn affects
    condition codes.

    Used when to eliminate test insns before a condition-code user,
    such as a "scc" insn or a conditional branch.  This includes
    checking if the entities that cc was updated by, are changed by the
    operation.

    Currently a jumble of the old peek-inside-the-insn and the newer
    check-cc-attribute methods.  */

void
cris_notice_update_cc (exp, insn)
     rtx exp;
     rtx insn;
{
  enum attr_cc attrval = get_attr_cc (insn);

  /* Check if user specified "-mcc-init" as a bug-workaround.  Remember
     to still set CC_REVERSED as below, since that's required by some
     compare insn alternatives.  (FIXME: GCC should do this virtual
     operand swap by itself.)  A test-case that may otherwise fail is
     gcc.c-torture/execute/20000217-1.c -O0 and -O1.  */
  if (TARGET_CCINIT)
    {
      CC_STATUS_INIT;

      if (attrval == CC_REV)
	cc_status.flags = CC_REVERSED;

      return;
    }

  attrval = get_attr_cc (insn);

  /* Slowly, we're converting to using attributes to control the setting
     of condition-code status.  */
  switch (attrval)
    {
    case CC_NONE:
      /* Even if it is "none", a setting may clobber a previous
	 cc-value, so check.  */
      if (GET_CODE (exp) == SET)
	{
	  if (cc_status.value1
	      && cris_reg_overlap_mentioned_p (SET_DEST (exp),
					     cc_status.value1))
	    cc_status.value1 = 0;

	  if (cc_status.value2
	      && cris_reg_overlap_mentioned_p (SET_DEST (exp),
					     cc_status.value2))
	    cc_status.value2 = 0;
	}
      return;

    case CC_CLOBBER:
      CC_STATUS_INIT;
      break;

    case CC_REV:
    case CC_NOOV32:
    case CC_NORMAL:
      cris_normal_notice_update_cc (exp);

      /* The "test" insn doesn't clear (carry and) overflow on V32.  We
	 can change bge => bpl and blt => bmi by passing on to the cc0
	 user that V should not be considered; bgt and ble are taken
	 care of by other methods (see {tst,cmp}{si,hi,qi}).  */
      if (attrval == CC_NOOV32 && TARGET_V32)
	cc_status.flags |= CC_NO_OVERFLOW;
      return;

    default:
      /* Unknown cc_attr value.  */
      abort ();
    }

  CC_STATUS_INIT;
}

/* Return != 0 if the return sequence for the current function is short,
   like "ret" or "jump [sp+]".  Prior to reloading, we can't tell how
   many registers must be saved, so return 0 then.  */

int
cris_simple_epilogue ()
{
  int regno;
  int reglimit = STACK_POINTER_REGNUM;
  int lastreg = -1;
  int got_really_used = cris_got_really_used ();

  cris_calculate_frame_layout (0);

  if (! reload_completed
      || frame_pointer_needed
      || get_frame_size () != 0
      || current_function_pretend_args_size
      || current_function_args_size
      || current_function_outgoing_args_size
      || current_function_calls_eh_return

      /* Kludge for 3.1: when reorg changes branches to the return label
	 into return insns, it does not handle the case where there's a
	 delay list for the epilogue: it just drops the insns in
	 current_function_epilogue_delay_list on the floor, resulting in
	 invalid code.  We kludge around it in that case by saying that
	 we don't have a simple enough epilogue to use return insns.  */
      || current_function_epilogue_delay_list != NULL

      /* We are more truthful for V32: assumptions on the "return"
	 expansion demand a single instruction.  No SRP restore.  */
      || (TARGET_V32
	  && (regs_ever_live [CRIS_SRP_REGNUM]
	      || cfun->machine->has_return_address_on_stack != 0))

      /* If we're not supposed to emit prologue and epilogue, we must
	 not emit return-type instructions.  */
      || !TARGET_PROLOGUE_EPILOGUE
      || TARGET_V10_V32_COMPATIBLE)
    return 0;

  /* We allow a "movem [sp+],rN" to sit in front if the "jump [sp+]" or
     in the delay-slot of the "ret".  */
  for (regno = 0; regno < reglimit; regno++)
    if (cris_reg_saved_in_regsave_area (regno, got_really_used))
      {
	/* No restore insns allowed for V32.  */
	if (TARGET_V32)
	  return 0;

	if (lastreg != regno - 1)
	  return 0;
	lastreg = regno;
      }

  return 1;
}

/* The ADDRESS_COST worker.  */

int
cris_address_cost (x)
     rtx x;
{
  /* The metric to use for the cost-macros is unclear.
     The metric used here is (the number of cycles needed) / 2,
     where we consider equal a cycle for a word of code and a cycle to
     read memory.  */

  /* The cheapest addressing modes get 0, since nothing extra is needed.  */
  if (BASE_OR_AUTOINCR_P (x))
    return 0;

  /* An indirect mem must be a DIP.  This means two bytes extra for code,
     and 4 bytes extra for memory read, i.e.  (2 + 4) / 2.  */
  if (GET_CODE (x) == MEM)
    return (2 + 4) / 2;

  /* Assume (2 + 4) / 2 for a single constant; a dword, since it needs
     an extra DIP prefix and 4 bytes of constant in most cases.  */
  if (CONSTANT_P (x))
    return (2 + 4) / 2;

  /* Handle BIAP and BDAP prefixes.  */
  if (GET_CODE (x) == PLUS)
    {
      rtx tem1 = XEXP (x, 0);
      rtx tem2 = XEXP (x, 1);

    /* A BIAP is 2 extra bytes for the prefix insn, nothing more.  We
       recognize the typical MULT which is always in tem1 because of
       insn canonicalization.  */
    if ((GET_CODE (tem1) == MULT && BIAP_INDEX_P (tem1))
	|| REG_P (tem1))
      return 2 / 2;

    /* A BDAP (quick) is 2 extra bytes.  Any constant operand to the
       PLUS is always found in tem2.  */
    if (GET_CODE (tem2) == CONST_INT
	&& INTVAL (tem2) < 128 && INTVAL (tem2) >= -128)
      return 2 / 2;

    /* A BDAP -32768 .. 32767 is like BDAP quick, but with 2 extra
       bytes.  */
    if (GET_CODE (tem2) == CONST_INT
	&& CONST_OK_FOR_LETTER_P (INTVAL (tem2), 'L'))
      return (2 + 2) / 2;

    /* A BDAP with some other constant is 2 bytes extra.  */
    if (CONSTANT_P (tem2))
      return (2 + 2 + 2) / 2;

    /* BDAP with something indirect should have a higher cost than
       BIAP with register.   FIXME: Should it cost like a MEM or more?  */
    /* Don't need to check it, it's the only one left.
       FIXME:  There was a REG test missing, perhaps there are others.
       Think more.  */
    return (2 + 2 + 2) / 2;
  }

  /* What else?  Return a high cost.  It matters only for valid
     addressing modes.  */
  return 10;
}

/* Check various objections to the side-effect.  Used in the test-part
   of an anonymous insn describing an insn with a possible side-effect.
   Returns nonzero if the implied side-effect is ok.

   code     : PLUS or MULT
   ops	    : An array of rtx:es. lreg, rreg, rval,
	      The variables multop and other_op are indexes into this,
	      or -1 if they are not applicable.
   lreg     : The register that gets assigned in the side-effect.
   rreg     : One register in the side-effect expression
   rval     : The other register, or an int.
   multop   : An integer to multiply rval with.
   other_op : One of the entities of the main effect,
	      whose mode we must consider.  */

int
cris_side_effect_mode_ok (code, ops, lreg, rreg, rval, multop, other_op)
     enum rtx_code code;
     rtx *ops;
     int lreg, rreg, rval, multop, other_op;
{
  /* Find what value to multiply with, for rx =ry + rz * n.  */
  int mult = multop < 0 ? 1 : INTVAL (ops[multop]);

  rtx reg_rtx = ops[rreg];
  rtx val_rtx = ops[rval];

  /* The operands may be swapped.  Canonicalize them in reg_rtx and
     val_rtx, where reg_rtx always is a reg (for this constraint to
     match).  */
  if (! BASE_P (reg_rtx))
    reg_rtx = val_rtx, val_rtx = ops[rreg];

  /* Don't forget to check that reg_rtx really is a reg.  If it isn't,
     we have no business.  */
  if (! BASE_P (reg_rtx))
    return 0;

  /* Don't do this when -mno-split.  */
  if (!TARGET_SIDE_EFFECT_PREFIXES)
    return 0;

  /* The mult expression may be hidden in lreg.  FIXME:  Add more
     commentary about that.  */
  if (GET_CODE (val_rtx) == MULT)
    {
      mult = INTVAL (XEXP (val_rtx, 1));
      val_rtx = XEXP (val_rtx, 0);
      code = MULT;
    }

  /* First check the "other operand".  */
  if (other_op >= 0)
    {
      if (GET_MODE_SIZE (GET_MODE (ops[other_op])) > UNITS_PER_WORD)
	return 0;

      /* Check if the lvalue register is the same as the "other
	 operand".  If so, the result is undefined and we shouldn't do
	 this.  FIXME:  Check again.  */
      if ((BASE_P (ops[lreg])
	   && BASE_P (ops[other_op])
	   && REGNO (ops[lreg]) == REGNO (ops[other_op]))
	  || rtx_equal_p (ops[other_op], ops[lreg]))
      return 0;
    }

  /* Do not accept frame_pointer_rtx as any operand.  */
  if (ops[lreg] == frame_pointer_rtx || ops[rreg] == frame_pointer_rtx
      || ops[rval] == frame_pointer_rtx
      || (other_op >= 0 && ops[other_op] == frame_pointer_rtx))
    return 0;

  if (code == PLUS
      && ! BASE_P (val_rtx))
    {
      /* Check allowed cases, like [r(+)?].[bwd] and const.  */
      if (CONSTANT_P (val_rtx))
	return 1;

      if (GET_CODE (val_rtx) == MEM
	  && BASE_OR_AUTOINCR_P (XEXP (val_rtx, 0)))
	return 1;

      if (GET_CODE (val_rtx) == SIGN_EXTEND
	  && GET_CODE (XEXP (val_rtx, 0)) == MEM
	  && BASE_OR_AUTOINCR_P (XEXP (XEXP (val_rtx, 0), 0)))
	return 1;

      /* If we got here, it's not a valid addressing mode.  */
      return 0;
    }
  else if (code == MULT
	   || (code == PLUS && BASE_P (val_rtx)))
    {
      /* Do not allow bad multiply-values.  */
      if (mult != 1 && mult != 2 && mult != 4)
	return 0;

      /* Only allow  r + ...  */
      if (! BASE_P (reg_rtx))
	return 0;

      /* If we got here, all seems ok.
	 (All checks need to be done above).  */
      return 1;
    }

  /* If we get here, the caller got its initial tests wrong.  */
  internal_error ("internal error: cris_side_effect_mode_ok with bad operands");
}

/* Whether next_cc0_user of insn is LE or GT or requires a real compare
   insn for other reasons.  */

int
cris_cc0_user_requires_cmp (insn)
     rtx insn;
{
  rtx cc0_user = NULL;
  rtx body;
  rtx set;

  if (insn == NULL)
    abort ();

  /* We always use a "cmp" in compatibility mode, so we don't have to
     keep track of what flags are valid after a compare insn.  */
  if (TARGET_V10_V32_COMPATIBLE)
    return 1;

  if (!TARGET_V32)
    return 0;

  cc0_user = next_cc0_user (insn);
  if (cc0_user == NULL)
    return 0;
      
  body = PATTERN (cc0_user);
  set = single_set (cc0_user);

  /* Users can be sCC and bCC.  */
  if (GET_CODE (cc0_user) == JUMP_INSN
      && GET_CODE (body) == SET
      && SET_DEST (body) == pc_rtx
      && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE
      && XEXP (XEXP (SET_SRC (body), 0), 0) == cc0_rtx)
    {
      return
	GET_CODE (XEXP (SET_SRC (body), 0)) == GT
	|| GET_CODE (XEXP (SET_SRC (body), 0)) == LE;
    }
  else if (set)
    {
      return
	GET_CODE (SET_SRC (body)) == GT
	|| GET_CODE (SET_SRC (body)) == LE;
    }

  abort ();
}

/* The function reg_overlap_mentioned_p in CVS (still as of 2001-05-16)
   does not handle the case where the IN operand is strict_low_part; it
   does handle it for X.  Test-case in Axis-20010516.  This function takes
   care of that for THIS port.  FIXME: strict_low_part is going away
   anyway.  */

static int
cris_reg_overlap_mentioned_p (x, in)
     rtx x, in;
{
  /* The function reg_overlap_mentioned now handles when X is
     strict_low_part, but not when IN is a STRICT_LOW_PART.  */
  if (GET_CODE (in) == STRICT_LOW_PART)
    in = XEXP (in, 0);

  return reg_overlap_mentioned_p (x, in);
}

/* The TARGET_ASM_NAMED_SECTION worker.
   We just dispatch to the functions for ELF and a.out.  */

void
cris_target_asm_named_section (name, flags)
     const char *name;
     unsigned int flags;
{
  if (! TARGET_ELF)
    default_no_named_section (name, flags);
  else
    default_elf_asm_named_section (name, flags);
}

/* The ASM_OUTPUT_CASE_END worker.  */
void
cris_asm_output_case_end (stream, num, table)
     FILE *stream;
     int num;
     rtx table;
{
  if (TARGET_V32)
    {
      rtx whole_jump_insn = PATTERN (PREV_INSN (PREV_INSN (table)));

      /* This can be a SEQUENCE, meaning the delay-slot of the jump is
         filled.  */
      rtx parallel_jump
	= (GET_CODE (whole_jump_insn) == SEQUENCE
	   ? PATTERN (XVECEXP (whole_jump_insn, 0, 0)) : whole_jump_insn);

      /* FIXME: Is this .word form ok or does it have to be the L1-L2
	 form to be recognized by the gas broken-dot-word machinery?  */
      asm_fprintf (stream,
		   "\t.word %LL%d-.\n",
		   CODE_LABEL_NUMBER (XEXP (XEXP (XEXP (XVECEXP
							(parallel_jump, 0, 0),
							1), 2), 0)),
		   (TARGET_PDEBUG ? "; default" : ""));
    }
  else
    asm_fprintf (stream,
		 "\t.word %LL%d-%LL%d%s\n",
		 CODE_LABEL_NUMBER (XEXP
				    (XEXP
				     (XEXP
				      (XVECEXP
					 (PATTERN
					  (PREV_INSN
					   (PREV_INSN (table))), 0, 0), 1),
				      2), 0)),
		 num,
		 (TARGET_PDEBUG ? "; default" : ""));
}

/* Return TRUE iff X is a CONST valid for e.g. indexing.  */

int
cris_valid_pic_const (x, any_operand)
     rtx x;
     int any_operand;
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONSTANT_P_RTX:
      return 1;

    default:
      ;
    }

  if (GET_CODE (x) != CONST)
    return 0;

  x = XEXP (x, 0);

  /* Handle (const (plus (unspec .. UNSPEC_GOTREL) (const_int ...))).  */
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == UNSPEC
      && (XINT (XEXP (x, 0), 1) == CRIS_UNSPEC_GOTREL
	  || XINT (XEXP (x, 0), 1) == CRIS_UNSPEC_PCREL)
      && GET_CODE (XEXP (x, 1)) == CONST_INT)
    x = XEXP (x, 0);

  if (GET_CODE (x) == UNSPEC)
    switch (XINT (x, 1))
      {
	/* A PCREL operand is only valid for call and movsi.  */
      case CRIS_UNSPEC_PLT_PCREL:
      case CRIS_UNSPEC_PCREL:
	return any_operand == 0;

      case CRIS_UNSPEC_PLT_GOTREL:
      case CRIS_UNSPEC_PLTGOTREAD:
      case CRIS_UNSPEC_GOTREAD:
      case CRIS_UNSPEC_GOTREL:
	return 1;
      default:
	abort ();
      }

  return cris_pic_symbol_type_of (x) == cris_no_symbol;
}

/* Helper function to find the right PIC-type symbol to generate,
   given the original (non-PIC) representation.  */

enum cris_pic_symbol_type
cris_pic_symbol_type_of (x)
     rtx x;
{
  switch (GET_CODE (x))
    {
    case SYMBOL_REF:
      return SYMBOL_REF_FLAG (x)
	? cris_rel_symbol : cris_got_symbol;

    case LABEL_REF:
      return cris_rel_symbol;

    case CONST:
      return cris_pic_symbol_type_of (XEXP (x, 0));

    case PLUS:
    case MINUS:
      {
	enum cris_pic_symbol_type t1 = cris_pic_symbol_type_of (XEXP (x, 0));
	enum cris_pic_symbol_type t2 = cris_pic_symbol_type_of (XEXP (x, 1));

	if (!(t1 == cris_no_symbol || t2 == cris_no_symbol))
	  abort ();

	/* We mustn't have offsets on GOT-symbols.  */
	if (t1 == cris_got_symbol || t2 == cris_got_symbol)
	  return cris_got_symbol_needing_fixup;

	return t1 != cris_no_symbol ? t1 : t2;
      }

    case CONST_INT:
    case CONST_DOUBLE:
    case CONSTANT_P_RTX:
      return cris_no_symbol;

    case UNSPEC:
      /* Likely an offsettability-test attempting to add a constant to
	 a GOTREAD symbol, which can't be handled.  */
      return cris_invalid_pic_symbol;

    default:
      fatal_insn ("unrecognized supposed constant", x);
    }

  abort ();
}

/* The LEGITIMATE_PIC_OPERAND_P worker.  */

int
cris_legitimate_pic_operand (x)
     rtx x;
{
  /* Symbols are not valid PIC operands as-is; just non-PCREL constants.  */
  return cris_valid_pic_const (x, 1);
}

/* The OVERRIDE_OPTIONS worker.
   As is the norm, this also parses -mfoo=bar type parameters.  */

void
cris_override_options ()
{
  if (cris_max_stackframe_str)
    {
      cris_max_stackframe = atoi (cris_max_stackframe_str);

      /* Do some sanity checking.  */
      if (cris_max_stackframe < 0 || cris_max_stackframe > 0x20000000)
	internal_error ("-max-stackframe=%d is not usable, not between 0 and %d",
			cris_max_stackframe, 0x20000000);
    }

  /* Let "-metrax4" and "-metrax100" change the cpu version.  */
  if (TARGET_SVINTO && cris_cpu_version < CRIS_CPU_SVINTO)
    cris_cpu_version = CRIS_CPU_SVINTO;
  else if (TARGET_ETRAX4_ADD && cris_cpu_version < CRIS_CPU_ETRAX4)
    cris_cpu_version = CRIS_CPU_ETRAX4;

  /* Parse -march=... and its synonym, the deprecated -mcpu=...  */
  if (cris_cpu_str)
    {
      cris_cpu_version
	= (*cris_cpu_str == 'v' ? atoi (cris_cpu_str + 1) : -1);

      if (strcmp ("etrax4", cris_cpu_str) == 0)
	cris_cpu_version = 3;

      if (strcmp ("svinto", cris_cpu_str) == 0
	  || strcmp ("etrax100", cris_cpu_str) == 0)
	cris_cpu_version = 8;

      if (strcmp ("ng", cris_cpu_str) == 0
	  || strcmp ("etrax100lx", cris_cpu_str) == 0)
	cris_cpu_version = 10;

      if (strcmp ("common_v10_v32", cris_cpu_str) == 0)
	{
	  cris_cpu_version = 10;

	  /* Carry isn't affected by move insns.  Simplest way to handle
	     that is to assume all insns clobber CC.  Prefixes aren't
	     used at all, so don't generate code with them having
	     side-effects.  */
	  target_flags
	    |= (TARGET_MASK_V10_V32_COMPATIBLE
		| TARGET_MASK_CCINIT);
	  target_flags
	    &= ~TARGET_MASK_SIDE_EFFECT_PREFIXES;

	  if (flag_pic)
	    error ("-march=common_v10_v32 not supported with -fpic or -fPIC");
	  flag_pic = 0;
	}

      if (cris_cpu_version < 0 || cris_cpu_version > 32)
	error ("unknown CRIS version specification in -march= or -mcpu= : %s",
	       cris_cpu_str);

      /* Set the target flags.  */
      if (cris_cpu_version >= CRIS_CPU_ETRAX4)
	target_flags |= TARGET_MASK_ETRAX4_ADD;

      /* If this is Svinto or higher, align for 32 bit accesses.  */
      if (cris_cpu_version >= CRIS_CPU_SVINTO)
	target_flags
	  |= (TARGET_MASK_SVINTO | TARGET_MASK_ALIGN_BY_32
	      | TARGET_MASK_STACK_ALIGN | TARGET_MASK_CONST_ALIGN
	      | TARGET_MASK_DATA_ALIGN);

      /* Note that we do not add new flags when it can be completely
	 described with a macro that uses -mcpu=X.  So
	 TARGET_HAS_MUL_INSNS is (cris_cpu_version >= CRIS_CPU_NG).  */
    }

  if (cris_tune_str)
    {
      int cris_tune
	= (*cris_tune_str == 'v' ? atoi (cris_tune_str + 1) : -1);

      if (strcmp ("etrax4", cris_tune_str) == 0)
	cris_tune = 3;

      if (strcmp ("svinto", cris_tune_str) == 0
	  || strcmp ("etrax100", cris_tune_str) == 0)
	cris_tune = 8;

      if (strcmp ("ng", cris_tune_str) == 0
	  || strcmp ("etrax100lx", cris_tune_str) == 0)
	cris_tune = 10;

      if (cris_tune < 0 || cris_tune > 10)
	error ("unknown CRIS cpu version specification in -mtune= : %s",
	       cris_tune_str);

      if (cris_tune >= CRIS_CPU_SVINTO)
	/* We have currently nothing more to tune than alignment for
	   memory accesses.  */
	target_flags
	  |= (TARGET_MASK_STACK_ALIGN | TARGET_MASK_CONST_ALIGN
	      | TARGET_MASK_DATA_ALIGN | TARGET_MASK_ALIGN_BY_32);
    }

  if (flag_pic)
    {
      /* Use error rather than warning, so invalid use is easily
	 detectable.  Still change to the values we expect, to avoid
	 further errors.  */
      if (! TARGET_LINUX)
	{
	  error ("-fPIC and -fpic are not supported in this configuration");
	  flag_pic = 0;
	}

      /* Turn off function CSE.  We need to have the addresses reach the
	 call expanders to get PLT-marked, as they could otherwise be
	 compared against zero directly or indirectly.  After visiting the
	 call expanders they will then be cse:ed, as the call expanders
	 force_reg the addresses, effectively forcing flag_no_function_cse
	 to 0.  */
      flag_no_function_cse = 1;
    }

  /* Scheduling insns before reload is a loser on this target.  */
  flag_schedule_insns = 0;

  if ((write_symbols == DWARF_DEBUG
       || write_symbols == DWARF2_DEBUG) && ! TARGET_ELF)
    {
      warning ("that particular -g option is invalid with -maout and -melinux");
      write_symbols = DBX_DEBUG;
    }
}

/* The ASM_OUTPUT_MI_THUNK worker.  */

void
cris_asm_output_mi_thunk (stream, thunkdecl, delta, funcdecl)
     FILE *stream;
     tree thunkdecl ATTRIBUTE_UNUSED;
     int delta;
     tree funcdecl;
{
  if (delta > 0)
    asm_fprintf (stream, "\tadd%s %d,$%s\n",
		 ADDITIVE_SIZE_MODIFIER (delta), delta,
		 reg_names[CRIS_FIRST_ARG_REG]);
  else if (delta < 0)
    asm_fprintf (stream, "\tsub%s %d,$%s\n",
		 ADDITIVE_SIZE_MODIFIER (-delta), -delta,
		 reg_names[CRIS_FIRST_ARG_REG]);

  if (flag_pic)
    {
      const char *name = XSTR (XEXP (DECL_RTL (funcdecl), 0), 0);

      /* We have no other relative (to either PC or GOT) reloc than one
	 that requests a PLT entry.  Since we don't set up the GOT
	 register, the jump absolutely must not be redirected through a
	 PLT entry.  We manage anyway by going through a local symbol.
	 This depends on the assembler to not short-circuit equalities
	 set by the ".set" directive (at least not for PIC relocs) and
	 on the linker to direct R_CRIS_32_PLT_PCREL relocs *directly*
	 to the symbol for local symbols, instead of through a PLT
	 entry.  */
      STRIP_NAME_ENCODING (name, name);
      fprintf (stream, TARGET_V32 ? "\tba " : "\tadd.d ");
      assemble_name (stream, name);
      fprintf (stream,
	       TARGET_V32
	       ? "..xth%s\n\tnop\n" : "..xth%s,$pc\n",
	       CRIS_PLT_PCOFFSET_SUFFIX);
      fprintf (stream, "\t.set ");
      assemble_name (stream, name);
      fprintf (stream, "..xth,");
      assemble_name (stream, name);
      fprintf (stream, "\n");
    }
  else
    {
      if (TARGET_V10_V32_COMPATIBLE)
	{
	  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (funcdecl))) != 0)
	    {
	      tree parm = TYPE_ARG_TYPES (TREE_TYPE (funcdecl));
	      for (; parm; parm = TREE_CHAIN (parm))
		if (TREE_VALUE (parm) == void_type_node)
		  break;

	      /* Fall back to usual call sequence and use R0 for the
		 address of the thunk.  Varargs can't be handled.  */
	      if (parm == NULL_TREE)
		sorry ("thunk calling function which uses `...' with -march=common_v10_v32");
	      else
		{
		  fprintf (stream, "\tsubq 4,$sp\n\tmove.d $r0,[$sp]\n");
		  fprintf (stream,
			   "\tsubq 4,$sp\n\tmove $srp,[$sp]\n\tmove.d ");
		  assemble_name (stream,
				 XSTR (XEXP (DECL_RTL (funcdecl), 0), 0));
		  fprintf (stream, ",$r0\n\tjsr $r0\n\tsetf\n\tmove.d [$sp+],$r13\n\t");
		  fprintf (stream, "move.d [$sp+],$r0\n\tjump $r13\n\tsetf\n");
		}
	    }
	  else
	    {
	      fprintf (stream, "\tmove.d ");
	      assemble_name (stream, XSTR (XEXP (DECL_RTL (funcdecl), 0), 0));
	      fprintf (stream, ",$r9\n\tjump $r9\n\tsetf\n");
	    }
	}
      else
	{
	  fprintf (stream, "\tjump ");
	  assemble_name (stream, XSTR (XEXP (DECL_RTL (funcdecl), 0), 0));
	  fprintf (stream, "\n");

	  if (TARGET_V32)
	    fprintf (stream, "\tnop\n");
	}
    }
}

/* The SETUP_INCOMING_VARARGS worker.  */

int
cris_setup_incoming_varargs (argssf, mode, type, second)
     CUMULATIVE_ARGS *argssf;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     tree type;
     int second ATTRIBUTE_UNUSED;
{
  int retval = 0;
  CUMULATIVE_ARGS args_at_unnamed;

  args_at_unnamed = *argssf;

  /* We skip over the last named argument, which GCC tries to put on the
     stack for stdarg functions (but not for vargargs), since we don't
     need it there; it can stay in the register.  This might look like
     an optimization but is actually necessary, lest we make sure the
     alias set for saving registers for the last named (and possibly
     unnamed) parameters in the expanded function prologue is *not* the
     varargs set.  That's because the last named parameter is retrieved
     using the set corresponding to the named parameter.  Then
     scheduling comes along, and can swap the store (in the varargs
     set) and the load (in the parameter type set).  This happened for
     gcc.c-torture/execute/20031219-Axis1.c.  */
  if (current_function_stdarg)
    FUNCTION_ARG_ADVANCE (args_at_unnamed, mode, type, 1);

  if (args_at_unnamed.regs < (CRIS_MAX_ARGS_IN_REGS))
    {
      /* We note the number of regs saved for varargs usage in a
	 machine-specific location, since
	 current_function_pretend_args_size isn't reliable for that; it
	 might be set for other usage, like FUNCTION_ARG_PARTIAL_NREGS.
	 We don't need to bother with the actual data; that data is
	 saved by whoever set current_function_pretend_args_size.  */
      int stdarg_regs
	= (CRIS_MAX_ARGS_IN_REGS) - args_at_unnamed.regs;
      cfun->machine->stdarg_regs = stdarg_regs;
      retval = stdarg_regs * 4; 
    }

  if (TARGET_PDEBUG)
    {
      fprintf (asm_out_file,
	       "\n; VA:: %s: %d args before, anon @ #%d, %dtime\n",
	       current_function_varargs ? "OLD" : "ANSI",
	       args_at_unnamed.regs, retval, second);
    }

  return retval;
}

/* The FUNCTION_OK_FOR_SIBCALL worker.  */

int
cris_function_ok_for_sibcall (decl)
     tree decl ATTRIBUTE_UNUSED;
{
  /* See sparc.h for the current_function_returns_struct part.  We can't
     restrict by checking cris_simple_epilogue (), because that only
     returns nonzero after reload.  Supposedly we can only do better
     with the complete epilogue in front of a jump, saving the SRP
     handling, so that's what we do.  A few restrictions are necessary;
     we can never do this for PIC calls that go through the GOT (though
     we handle that by just excluding all PIC calls) and not for
     functions that call eh_return (too much of a corner case to be
     worth the risk), and sometimes we need R9 in the sibcall epilogue,
     so we'd rather not have it as input as well (FIXME: maybe that case
     is worthwhile).  */
  return
    !TARGET_V10_V32_COMPATIBLE
    && !current_function_returns_struct
    && !current_function_calls_eh_return
    && !flag_pic;
}

/* Helper functions for sibcalls.  */

void
cris_output_sibcall_epilogue ()
{
  if (!cris_function_ok_for_sibcall (NULL_TREE))
    abort ();

  if (TARGET_V32)
    return;

  cris_target_asm_function_epilogue_1 (asm_out_file, get_frame_size (), 1,
				       NULL);
}

/* The EXPAND_BUILTIN_VA_ARG worker.  This is modified from the
   "standard" implementation of va_arg: read the value from the current
   address and increment by the size of one or two registers.  The
   important difference for CRIS is that if the type is
   pass-by-reference, then perform an indirection.  */

rtx
cris_expand_builtin_va_arg (valist, type)
     tree valist;
     tree type;
{
  tree addr_tree, t;
  rtx addr;
  tree passed_size = size_zero_node;
  tree type_size = NULL;
  tree size3 = size_int (3);
  tree size4 = size_int (4);
  tree size8 = size_int (8);
  tree rounded_size;

  /* Get AP.  */
  addr_tree = valist;

  if (type == error_mark_node
      || (type_size = TYPE_SIZE_UNIT (TYPE_MAIN_VARIANT (type))) == NULL
      || TREE_OVERFLOW (type_size))
    /* Presumably an error; the size isn't computable.  A message has
       supposedly been emitted elsewhere.  */
    rounded_size = size_zero_node;
  else
    rounded_size
      = fold (build (MULT_EXPR, sizetype,
		     fold (build (TRUNC_DIV_EXPR, sizetype,
				  fold (build (PLUS_EXPR, sizetype,
					       type_size, size3)),
				  size4)),
		     size4));

  if (!integer_zerop (rounded_size))
    {
      /* Check if the type is passed by value or by reference.  Values up
	 to 8 bytes are passed by-value, padded to register-size (4
	 bytes).  Larger values and varying-size types are passed
	 by reference.  */
      passed_size
	= (!really_constant_p (type_size)
	   ? size4
	   : fold (build (COND_EXPR, sizetype,
			  fold (build (GT_EXPR, sizetype,
				       rounded_size,
				       size8)),
			  size4,
			  rounded_size)));

      addr_tree
	= (!really_constant_p (type_size)
	   ? build1 (INDIRECT_REF, build_pointer_type (type), addr_tree)
	   : fold (build (COND_EXPR, TREE_TYPE (addr_tree),
			  fold (build (GT_EXPR, sizetype,
				       rounded_size,
				       size8)),
			  build1 (INDIRECT_REF, build_pointer_type (type),
				  addr_tree),
			  addr_tree)));
    }

  addr = expand_expr (addr_tree, NULL_RTX, Pmode, EXPAND_NORMAL);
  addr = copy_to_reg (addr);

  if (!integer_zerop (rounded_size))
    {
      /* Compute new value for AP.  */
      t = build (MODIFY_EXPR, TREE_TYPE (valist), valist,
		 build (PLUS_EXPR, TREE_TYPE (valist), valist,
			passed_size));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  return addr;
}

/* The INIT_EXPANDERS worker.  We need to allocate cfun->machine and
   mark GC:ed data there.  */

void
cris_init_expanders ()
{
  /* Set the per-function-data initializer.  */
  init_machine_status = cris_init_machine_status;

  mark_machine_status = cris_mark_machine_status;
}

/* Zero initialization is OK for all current fields.  */

static void
cris_init_machine_status (p)
     struct function *p;
{
  p->machine = xcalloc (1, sizeof (struct machine_function));
}

/* Mark all GC:ed data in p->machine.  */

static void
cris_mark_machine_status (p)
     struct function *p;
{
  if (p->machine == NULL)
    return;
  
  ggc_mark_rtx (p->machine->srp_save_register);
}

/* Split a 2 word move (DI or presumably DF) into component parts.
   Originally a copy of gen_split_move_double in m32r.c.  */

rtx
cris_split_movdx (operands)
     rtx *operands;
{
  enum machine_mode mode = GET_MODE (operands[0]);
  rtx dest = operands[0];
  rtx src  = operands[1];
  rtx val;

  /* We used to have to handle (SUBREG (MEM)) here, but that should no
     longer happen; after reload there are no SUBREGs any more, and we're
     only called after reload.  */
  if (GET_CODE (dest) == SUBREG || GET_CODE (src) == SUBREG)
    abort ();

  start_sequence ();
  if (GET_CODE (dest) == REG)
    {
      int dregno = REGNO (dest);

      /* Reg-to-reg copy.  */
      if (GET_CODE (src) == REG)
	{
	  int sregno = REGNO (src);

	  int reverse = (dregno == sregno + 1);

	  /* We normally copy the low-numbered register first.  However, if
	     the first register operand 0 is the same as the second register of
	     operand 1, we must copy in the opposite order.  */
	  emit_insn (gen_rtx_SET (VOIDmode,
				  operand_subword (dest, reverse, TRUE, mode),
				  operand_subword (src, reverse, TRUE, mode)));

	  emit_insn (gen_rtx_SET (VOIDmode,
				  operand_subword (dest, !reverse, TRUE, mode),
				  operand_subword (src, !reverse, TRUE, mode)));
	}
      /* Constant-to-reg copy.  */
      else if (GET_CODE (src) == CONST_INT || GET_CODE (src) == CONST_DOUBLE)
	{
	  rtx words[2];
	  split_double (src, &words[0], &words[1]);
	  emit_insn (gen_rtx_SET (VOIDmode,
				  operand_subword (dest, 0, TRUE, mode),
				  words[0]));

	  emit_insn (gen_rtx_SET (VOIDmode,
				  operand_subword (dest, 1, TRUE, mode),
				  words[1]));
	}
      /* Mem-to-reg copy.  */
      else if (GET_CODE (src) == MEM)
	{
	  /* If the high-address word is used in the address, we must load it
	     last.  Otherwise, load it first.  */
	  rtx addr = XEXP (src, 0);
	  int reverse
	    = (refers_to_regno_p (dregno, dregno + 1, addr, NULL) != 0);

	  /* The original code imples that we can't do
	     move.x [rN+],rM  move.x [rN],rM+1
	     when rN is dead, because of REG_NOTES damage.  That is
	     consistent with what I've seen, so don't try it.

             We have two different cases here; if the addr is POST_INC,
             just pass it through, otherwise add constants.  */

          if (GET_CODE (addr) == POST_INC)
	    {
	      emit_insn (gen_rtx_SET (VOIDmode,
				      operand_subword (dest, 0, TRUE, mode),
				      change_address (src, SImode, addr)));
	      emit_insn (gen_rtx_SET (VOIDmode,
				      operand_subword (dest, 1, TRUE, mode),
				      change_address (src, SImode, addr)));
	    }
	  else
	    {
	      /* Make sure we don't get any other addresses with
		 embedded postincrements.  They should be stopped in
		 GO_IF_LEGITIMATE_ADDRESS, but we're here for your
		 safety.  */
	      if (side_effects_p (addr))
		fatal_insn ("unexpected side-effects in address", addr);

	      emit_insn (gen_rtx_SET
			 (VOIDmode,
			  operand_subword (dest, reverse, TRUE, mode),
			  change_address
			  (src, SImode,
			   plus_constant (addr,
					  reverse * UNITS_PER_WORD))));
	      emit_insn (gen_rtx_SET
			 (VOIDmode,
			  operand_subword (dest, ! reverse, TRUE, mode),
			  change_address
			  (src, SImode,
			   plus_constant (addr,
					  (! reverse) *
					  UNITS_PER_WORD))));
	    }
	}
      else
	abort ();
    }
  /* Reg-to-mem copy or clear mem.  */
  else if (GET_CODE (dest) == MEM
	   && (GET_CODE (src) == REG
	       || src == const0_rtx
	       || src == CONST0_RTX (DFmode)))
    {
      rtx addr = XEXP (dest, 0);

      if (GET_CODE (addr) == POST_INC)
	{
	  emit_insn (gen_rtx_SET (VOIDmode,
				  change_address (dest, SImode, addr),
				  operand_subword (src, 0, TRUE, mode)));
	  emit_insn (gen_rtx_SET (VOIDmode,
				  change_address (dest, SImode, addr),
				  operand_subword (src, 1, TRUE, mode)));
	}
      else
	{
	  /* Make sure we don't get any other addresses with embedded
	     postincrements.  They should be stopped in
	     GO_IF_LEGITIMATE_ADDRESS, but we're here for your safety.  */
	  if (side_effects_p (addr))
	    fatal_insn ("unexpected side-effects in address", addr);

	  emit_insn (gen_rtx_SET
		     (VOIDmode,
		      change_address (dest, SImode, addr),
		      operand_subword (src, 0, TRUE, mode)));

	  emit_insn (gen_rtx_SET
		     (VOIDmode,
		      change_address (dest, SImode,
				      plus_constant (addr,
						     UNITS_PER_WORD)),
		      operand_subword (src, 1, TRUE, mode)));
	}
    }

  else
    abort ();

  val = gen_sequence ();
  end_sequence ();
  return val;
}

/* Worker function for "return" pattern.  Returns non-zero if all is
   done.  Apparently must not be used as long as GCC doesn't handle
   multiple insns expanded by (return).  See
   gcc.c-torture/execute/20001130-2.c, at -O2 and above, flow2.  */

int
cris_expand_return ()
{
  if (TARGET_V32)
    {
      int i;
      int got_really_used = cris_got_really_used ();

      /* Restore call-saved registers.  Start from the last call-saved
	 register.  We know that we have a simple epilogue, so we just
	 have to find the last register in the movem sequence.  */

      for (i = 8; i >= 0; i--)
        if (regs_ever_live[i]
	    || (i == (int) PIC_OFFSET_TABLE_REGNUM && got_really_used))
	  abort ();

      if (i != -1)
	{
	  rtx mem
	    = gen_rtx_MEM (SImode,
			   gen_rtx_POST_INC (SImode, stack_pointer_rtx));
	  abort ();
	  set_mem_alias_set (mem, cris_get_sr_alias_set ());
	  emit_insn (gen_crisv32_movem_load (mem, GEN_INT (i + 1), 0));
	}

      /* Restore SRP, if saved on stack.  */
      if (!TARGET_V32 && regs_ever_live[CRIS_SRP_REGNUM])
	abort ();

      emit_jump_insn (gen_cris_return_v32 (gen_rtx_raw_REG (Pmode, CRIS_SRP_REGNUM)));
      return 1;
    }

  /* At the moment, we expand the "(return)" part too.  */
  return 0;
}

/* The expander for the prologue pattern name.  */

void
cris_expand_prologue ()
{
  int regno;
  int size = get_frame_size ();
  /* Shorten the used name for readability.  */
  int cfoa_size = current_function_outgoing_args_size;
  int last_movem_reg = -1;
  int framesize = 0;
  rtx mem, insn;
  int return_address_on_stack;
  int got_really_used = cris_got_really_used ();
  int n_movem_regs = 0;

  cris_calculate_frame_layout (0);

  return_address_on_stack
    = ((!TARGET_V32 && regs_ever_live[CRIS_SRP_REGNUM])
       || cfun->machine->has_return_address_on_stack != 0);

  /* Don't do anything if no prologues or epilogues are wanted.  */
  if (!TARGET_PROLOGUE_EPILOGUE)
    return;

  if (size < 0)
    abort ();

  /* Align the size to what's best for the CPU model.  */
  if (TARGET_STACK_ALIGN)
    size = TARGET_ALIGN_BY_32 ? (size + 3) & ~3 : (size + 1) & ~1;

  if (current_function_pretend_args_size)
    {
      int pretend = current_function_pretend_args_size;
      int stdarg_regs = cfun->machine->stdarg_regs;
      for (regno = CRIS_FIRST_ARG_REG + CRIS_MAX_ARGS_IN_REGS - 1;
	   /* See cris_setup_incoming_varargs.  */
	   stdarg_regs > 0;
	   regno--, pretend -= 4, stdarg_regs--)
	{
	  insn = emit_insn (gen_rtx_SET (VOIDmode,
					 stack_pointer_rtx,
					 plus_constant (stack_pointer_rtx,
							-4)));
	  /* FIXME: When dwarf2 frame output and unless asynchronous
	     exceptions, make dwarf2 bundle together all stack
	     adjustments like it does for registers between stack
	     adjustments (??? which it does for asynchronous exceptions
	     too?).  */
	  RTX_FRAME_RELATED_P (insn) = 1;

	  mem = gen_rtx_MEM (SImode, stack_pointer_rtx);
	  /* See cris_setup_incoming_varargs.  */
	  set_mem_alias_set (mem, get_varargs_alias_set ());
	  insn = emit_move_insn (mem, gen_rtx_raw_REG (SImode, regno));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      /* For other setters of current_function_pretend_args_size, we
	 just adjust the stack.  */
      if (pretend)
	insn = emit_insn (gen_rtx_SET (VOIDmode,
				       stack_pointer_rtx,
				       plus_constant (stack_pointer_rtx,
						      -pretend)));
    }

  framesize += current_function_pretend_args_size;

  /* Save SRP if not a leaf function.  For V32, we save the
     return-address this way only when there are no call-saved registers
     available.  */
  if (return_address_on_stack)
    {
      insn = emit_insn (gen_rtx_SET (VOIDmode,
				     stack_pointer_rtx,
				     plus_constant (stack_pointer_rtx, -4)));
      RTX_FRAME_RELATED_P (insn) = 1;

      mem = gen_rtx_MEM (SImode, stack_pointer_rtx);
      set_mem_alias_set (mem, get_frame_alias_set ());
      insn = emit_move_insn (mem, gen_rtx_raw_REG (SImode, CRIS_SRP_REGNUM));
      RTX_FRAME_RELATED_P (insn) = 1;
      framesize += 4;
    }

  /* Set up frame pointer if needed.  */
  if (frame_pointer_needed)
    {
      insn = emit_insn (gen_rtx_SET (VOIDmode,
				     stack_pointer_rtx,
				     plus_constant (stack_pointer_rtx, -4)));
      RTX_FRAME_RELATED_P (insn) = 1;

      mem = gen_rtx_MEM (SImode, stack_pointer_rtx);
      set_mem_alias_set (mem, get_frame_alias_set ());
      insn = emit_move_insn (mem, frame_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;

      insn = emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;

      framesize += 4;
    }

  /* Get a contiguous sequence of registers, starting with r0, that need
     to be saved.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      if (cris_reg_saved_in_regsave_area (regno, got_really_used))
	{
	  n_movem_regs++;

	  /* Check if movem may be used for registers so far.  For V32,
	     movem is cheap enough that we want to use it even if
	     there's a gap (there should be no gap; any gap in the
	     absence of specific registers mentioned in the source or
	     options is really a gcc bug).  */
	  if (((TARGET_V32 && !current_function_calls_eh_return)
	       || regno == last_movem_reg + 1)
	      && !TARGET_V10_V32_COMPATIBLE)
	    /* Yes, update next expected register.  */
	    last_movem_reg = regno;
	  else
	    {
	      /* We cannot use movem for all registers.  We have to flush
		 any movem:ed registers we got so far.  */
	      if (last_movem_reg != -1)
		{
		  int n_saved_regs
		    = (n_movem_regs == 1) ? 1 : last_movem_reg + 1;

		  insn
		    = emit_insn (gen_rtx_SET (VOIDmode,
					      stack_pointer_rtx,
					      plus_constant (stack_pointer_rtx,
							     -(n_saved_regs * 4
							       + size))));
		  RTX_FRAME_RELATED_P (insn) = 1;

		  mem = gen_rtx_MEM (SImode, stack_pointer_rtx);
		  set_mem_alias_set (mem, cris_get_sr_alias_set ());
		  if (n_saved_regs == 1)
		    insn = emit_move_insn (mem,
					   gen_rtx_raw_REG (SImode,
							    last_movem_reg));
		  else
		    {
		      insn = gen_crisv32_movem_store (mem,
						      GEN_INT (n_saved_regs),
						      1);
		      insn = emit_insn (insn);
		    }
		  RTX_FRAME_RELATED_P (insn) = 1;

		  framesize += n_saved_regs * 4 + size;
		  last_movem_reg = -1;
		  size = 0;
		}
	      else if (size > 0)
		{
		  /* FIXME: Merge this with the SET below.  */
		  insn
		    = emit_insn (gen_rtx_SET (VOIDmode,
					      stack_pointer_rtx,
					      plus_constant (stack_pointer_rtx,
							     -size)));
		  RTX_FRAME_RELATED_P (insn) = 1;
		  framesize += size;
		  size = 0;
		}

	      insn = emit_insn (gen_rtx_SET (VOIDmode,
					     stack_pointer_rtx,
					     plus_constant (stack_pointer_rtx,
							    -4)));
	      RTX_FRAME_RELATED_P (insn) = 1;

	      mem = gen_rtx_MEM (SImode, stack_pointer_rtx);
	      set_mem_alias_set (mem, cris_get_sr_alias_set ());
	      insn = emit_move_insn (mem, gen_rtx_raw_REG (SImode, regno));
	      RTX_FRAME_RELATED_P (insn) = 1;

	      framesize += 4;
	    }
	}
    }

  /* Check after, if we can movem all registers.  This is the normal
     case.  */
  if (last_movem_reg != -1)
    {
      int n_saved_regs
	= (n_movem_regs == 1) ? 1 : last_movem_reg + 1;

      insn
	= emit_insn (gen_rtx_SET (VOIDmode,
				  stack_pointer_rtx,
				  plus_constant (stack_pointer_rtx,
						 -(n_saved_regs * 4
						   + size))));
      RTX_FRAME_RELATED_P (insn) = 1;

      mem = gen_rtx_MEM (SImode, stack_pointer_rtx);
      set_mem_alias_set (mem, cris_get_sr_alias_set ());
      if (n_saved_regs == 1)
	insn = emit_move_insn (mem, gen_rtx_raw_REG (SImode, last_movem_reg));
      else
	{
	  insn
	    = gen_crisv32_movem_store (mem, GEN_INT (last_movem_reg + 1), 1);
	  insn = emit_insn (insn);
	}
      RTX_FRAME_RELATED_P (insn) = 1;

      framesize += n_saved_regs * 4 + size;
      /* We have to put outgoing argument space after regs.  */
      if (cfoa_size)
	{
	  insn = emit_insn (gen_rtx_SET (VOIDmode,
					 stack_pointer_rtx,
					 plus_constant (stack_pointer_rtx,
							-cfoa_size)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  framesize += cfoa_size;
	}
    }
  else if ((size + cfoa_size) > 0)
    {
      insn = emit_insn (gen_rtx_SET (VOIDmode,
				     stack_pointer_rtx,
				     plus_constant (stack_pointer_rtx,
						    -(cfoa_size + size))));
      RTX_FRAME_RELATED_P (insn) = 1;
      framesize += size + cfoa_size;
    }

  if (cfun->machine->srp_save_register)
    {
      rtx insn
	= emit_move_insn (cfun->machine->srp_save_register,
			  gen_rtx_raw_REG (SImode,
					   CRIS_SRP_REGNUM));
      RTX_FRAME_RELATED_P (insn) = 1;

      /* Allow the insn to be deleted for code where exceptions do not
	 pass through; they may be calling noreturn functions only.  */
      if (!flag_exceptions)
	REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD,
					      const0_rtx,
					      REG_NOTES (insn));
    }

  /* Set up the PIC register, if it is used.  */
  if (got_really_used)
    {
      rtx got
	= gen_rtx_UNSPEC (SImode, gen_rtvec (1, const0_rtx), CRIS_UNSPEC_GOT);
      emit_move_insn (pic_offset_table_rtx, got);

      /* FIXME: This is a cover-up for flow2 messing up; it doesn't
	 follow exceptional paths and tries to delete the GOT load as
	 unused, if it isn't used on the non-exceptional paths.  Other
	 ports have similar or other cover-ups, or plain bugs marking
	 the GOT register load as maybe-dead.  To see this, remove the
	 line below and try libsupc++/vec.cc or a trivial
	 "static void y (); void x () {try {y ();} catch (...) {}}".  */
      emit_insn (gen_rtx_USE (VOIDmode, pic_offset_table_rtx));
    }
  if (cris_max_stackframe && framesize > cris_max_stackframe)
    warning ("stackframe too big: %d bytes", framesize);
}

/* The expander for the epilogue pattern name.  */

void
cris_expand_epilogue (for_sibcall)
     int for_sibcall;
{
  int regno;
  int size = get_frame_size ();
  int last_movem_reg = -1;
  int argspace_offset = current_function_outgoing_args_size;
  int pretend =	 current_function_pretend_args_size;
  rtx mem;
  int return_address_on_stack
    = ((!TARGET_V32 && regs_ever_live[CRIS_SRP_REGNUM])
       || cfun->machine->has_return_address_on_stack != 0);
  /* A reference may have been optimized out
     (like the abort () in fde_split in unwind-dw2-fde.c, at least 3.2.1)
     so check that it's still used.  */
  int got_really_used = cris_got_really_used ();
  rtx srp_restore = cfun->machine->srp_save_register;
  int n_movem_regs = 0;

  if (!TARGET_PROLOGUE_EPILOGUE || !TARGET_V32)
    return;

  /* Align byte count of stack frame.  */
  if (TARGET_STACK_ALIGN)
    size = TARGET_ALIGN_BY_32 ? (size + 3) & ~3 : (size + 1) & ~1;

  /* Check how many saved regs we can movem.  They start at r0 and must
     be contiguous.  */
  for (regno = 0;
       regno < FIRST_PSEUDO_REGISTER;
       regno++)
    if (cris_reg_saved_in_regsave_area (regno, got_really_used))
      {
	n_movem_regs++;

	if (((TARGET_V32 && !current_function_calls_eh_return)
	     || regno == last_movem_reg + 1)
	    && !TARGET_V10_V32_COMPATIBLE)
	  last_movem_reg = regno;
	else
	  break;
      }

  /* If there was only one register that really needed to be saved
     through movem, don't use movem.  If there was only one register,
     then it's unlikely that we saved the return address in a register,
     because we only do that if when we calculate the layout, there are
     already movem:ed registers.  */
  if (n_movem_regs == 1)
    last_movem_reg = -1;

  /* Now emit "normal" move insns for all regs higher than the movem
     regs.  */
  for (regno = FIRST_PSEUDO_REGISTER - 1;
       regno > last_movem_reg;
       regno--)
    if (cris_reg_saved_in_regsave_area (regno, got_really_used))
      {
	/* Don't clobber the register where SRP is saved.  */
	if (srp_restore != NULL && regno == (int) REGNO (srp_restore))
	  {
	    emit_move_insn (gen_rtx_raw_REG (SImode, CRIS_SRP_REGNUM),
			    srp_restore);
	    srp_restore = NULL_RTX;
	  }

	if (argspace_offset)
	  {
	    /* There is an area for outgoing parameters located before
	       the saved registers.  We have to adjust for that.  */
	    emit_insn (gen_rtx_SET (VOIDmode,
				    stack_pointer_rtx,
				    plus_constant (stack_pointer_rtx,
						   argspace_offset)));
	    /* Make sure we only do this once.  */
	    argspace_offset = 0;
	  }

	mem = gen_rtx_MEM (SImode, gen_rtx_POST_INC (SImode,
						     stack_pointer_rtx));
	set_mem_alias_set (mem, cris_get_sr_alias_set ());
	emit_move_insn (gen_rtx_raw_REG (SImode, regno), mem);
      }

  /* If we have any movem-restore, do it now.  */
  if (last_movem_reg != -1)
    {
      if (argspace_offset)
	{
	  emit_insn (gen_rtx_SET (VOIDmode,
				  stack_pointer_rtx,
				  plus_constant (stack_pointer_rtx,
						 argspace_offset)));
	  argspace_offset = 0;
	}

      mem = gen_rtx_MEM (SImode, gen_rtx_POST_INC (SImode, stack_pointer_rtx));
      set_mem_alias_set (mem, cris_get_sr_alias_set ());

      /* Try and return through a jump to the register where SRP is
	 saved.  FIXME: This duplicates lots of tests from below.
	 FIXME: For V32, by including R9 when movem to R9 and size +
	 pretend == 4 or R10..R13 when size + pretend == 8..20 and no
	 return-value, we can avoid separate stack-pointer adjustments.
	 For pre-V32, similar for argspace_offset (unlikely noticeable,
	 though).  */
      if (!frame_pointer_needed
	  && size == 0
	  && !current_function_calls_eh_return
	  && srp_restore != NULL_RTX
	  && pretend == 0
	  /* No return insn if sibcall.  */
	  && !for_sibcall)
	{
	  rtx insn
	    = gen_crisv32_movem_load (mem, GEN_INT (last_movem_reg + 1), 2);
	  XVECEXP (insn, 0, 0) = gen_rtx_RETURN (VOIDmode);
	  XVECEXP (insn, 0, 1) = gen_rtx_USE (VOIDmode, srp_restore);
	  emit_jump_insn (insn);
	  return;
	}
      else
	{
	  if (srp_restore != NULL)
	    {
	      emit_move_insn (gen_rtx_raw_REG (SImode, CRIS_SRP_REGNUM),
			      srp_restore);
	      srp_restore = NULL_RTX;
	    }

	  emit_insn (gen_crisv32_movem_load (mem,
					     GEN_INT (last_movem_reg + 1), 0));
	}
    }

  /* If we don't clobber all of the allocated stack area (we've already
     deallocated saved registers), GCC might want to schedule loads from
     the stack to *after* the stack-pointer restore, which introduces an
     interrupt race condition.  This happened for the initial-value
     SRP-restore for g++.dg/eh/registers1.C (noticed by inspection of
     other failure for that test).  It also happened for the stack slot
     for the return value in (one version of)
     linux/fs/dcache.c:__d_lookup, at least with "-O2
     -fno-omit-frame-pointer".  */

  /* Restore frame pointer if necessary.  */
  if (frame_pointer_needed)
    {
      emit_insn (gen_cris_frame_deallocated_barrier ());

      emit_move_insn (stack_pointer_rtx, frame_pointer_rtx);
      mem = gen_rtx_MEM (SImode, gen_rtx_POST_INC (SImode,
						   stack_pointer_rtx));
      set_mem_alias_set (mem, get_frame_alias_set ());
      emit_move_insn (frame_pointer_rtx, mem);
    }
  else if ((size + argspace_offset) != 0)
    {
      emit_insn (gen_cris_frame_deallocated_barrier ());

      /* If there was no frame-pointer to restore sp from, we must
	 explicitly deallocate local variables.  */

      /* Handle space for outgoing parameters that hasn't been handled
	 yet.  */
      size += argspace_offset;

      emit_insn (gen_rtx_SET (VOIDmode,
			      stack_pointer_rtx,
			      plus_constant (stack_pointer_rtx, size)));
    }

  /* If this function has no pushed register parameters
     (stdargs/varargs), and if it is not a leaf function, then we have
     the return address on the stack.  */
  if (return_address_on_stack && pretend == 0)
    {
      if (TARGET_V10_V32_COMPATIBLE)
	{

	  /* Must jump via register; a call-clobbered one.  We shouldn't
	     *generally* use R9, R10 or R11 since they're used for
	     return values.  R13 is generally safe.  It fails for one
	     case: where we return a non-struct larger than 12 bytes,
	     for example a DCmode (complex double).  Emit compile-time
	     error for that instead of generating faulty code.  Note
	     that the jump *may* have a delay slot.  */
	  if (!aggregate_value_p (TREE_TYPE (TREE_TYPE (cfun->decl)))
	      && (int_size_in_bytes (TREE_TYPE (TREE_TYPE (cfun->decl))) > 12))
	    sorry ("return-type larger than 12 bytes with\
 -march=common_v10_v32");
	  else if (current_function_calls_eh_return)
	    {
	      rtx mem;
	      rtx stackadjreg
		= gen_rtx_raw_REG (SImode, CRIS_STACKADJ_REG);
	      rtx srpreg = gen_rtx_raw_REG (SImode, CRIS_SRP_REGNUM);
	      mem = gen_rtx_MEM (SImode, gen_rtx_POST_INC (SImode,
							   stack_pointer_rtx));
	      set_mem_alias_set (mem, get_frame_alias_set ());
	      emit_move_insn (srpreg, mem);

	      emit_insn (gen_addsi3 (stack_pointer_rtx,
				     stack_pointer_rtx,
				     stackadjreg));

	      emit_move_insn (stackadjreg, srpreg);

	      /* FIXME: Do we need a real return here?  */
	      emit_jump_insn (gen_indirect_jump (stackadjreg));
	    }
	  else
	    {
	      rtx mem;
	      rtx jmpreg = gen_rtx_raw_REG (SImode, 13);
	      mem = gen_rtx_MEM (SImode,
				 gen_rtx_POST_INC (SImode,
						   stack_pointer_rtx));
	      set_mem_alias_set (mem, get_frame_alias_set ());
	      emit_move_insn (jmpreg, mem);

	      /* FIXME: Do we need a real return here?  */
	      emit_jump_insn (gen_indirect_jump (jmpreg));
	    }
	}
      else
	{
	  if (current_function_calls_eh_return)
	    {
	      rtx mem;
	      rtx srpreg = gen_rtx_raw_REG (SImode, CRIS_SRP_REGNUM);
	      mem = gen_rtx_MEM (SImode,
				 gen_rtx_POST_INC (SImode,
						   stack_pointer_rtx));
	      set_mem_alias_set (mem, get_frame_alias_set ());
	      emit_move_insn (srpreg, mem);

	      emit_insn (gen_addsi3 (stack_pointer_rtx,
				     stack_pointer_rtx,
				     gen_rtx_raw_REG (SImode,
						      CRIS_STACKADJ_REG)));

	      emit_jump_insn (TARGET_V32
			      ? gen_cris_return_v32 (srpreg)
			      : gen_rtx_RETURN (VOIDmode));
	    }
	  else
	    {
	      rtx mem;
	      rtx srpreg = gen_rtx_raw_REG (SImode, CRIS_SRP_REGNUM);
	      mem = gen_rtx_MEM (SImode,
				 gen_rtx_POST_INC (SImode,
						   stack_pointer_rtx));
	      set_mem_alias_set (mem, get_frame_alias_set ());
	      /* FIXME: How do we make sure these are combined to a
		 "jump [sp+]" for pre-V32?  */
	      emit_move_insn (srpreg, mem);

	      /* If for sibcall, we want no return insn.  */
	      if (for_sibcall)
		return;

	      emit_jump_insn (TARGET_V32
			      ? gen_cris_return_v32 (srpreg)
			      : gen_rtx_RETURN (VOIDmode));
	    }
	}
      return;
    }

  /* If we pushed some register parameters, then adjust the stack for
     them.  */
  if (pretend != 0)
    {
      /* If SRP is stored on the way, we need to restore it first.  */
      if (return_address_on_stack)
	{
	  rtx mem;
	  rtx srpreg = gen_rtx_raw_REG (SImode, CRIS_SRP_REGNUM);
	  mem = gen_rtx_MEM (SImode,
			     gen_rtx_POST_INC (SImode,
					       stack_pointer_rtx));
	  set_mem_alias_set (mem, get_frame_alias_set ());
	  emit_move_insn (srpreg, mem);
	}
      emit_insn (gen_rtx_SET (VOIDmode,
			      stack_pointer_rtx,
			      plus_constant (stack_pointer_rtx, pretend)));
    }

  /* Perform the "physical" unwinding that the EH machinery calculated.  */
  if (current_function_calls_eh_return)
    emit_insn (gen_addsi3 (stack_pointer_rtx,
			   stack_pointer_rtx,
			   gen_rtx_raw_REG (SImode,
					    CRIS_STACKADJ_REG)));

  /* If this was for a sibcall, then we won't emit any return insn.  */
  if (for_sibcall)
    return;

  if (TARGET_V10_V32_COMPATIBLE)
    {
      if (!aggregate_value_p (TREE_TYPE (TREE_TYPE (cfun->decl)))
	  && (int_size_in_bytes (TREE_TYPE (TREE_TYPE (cfun->decl)))
	      > 12))
	sorry ("return-type larger than 12 bytes with -march=common_v10_v32");

      emit_move_insn (gen_rtx_raw_REG (SImode, 13),
		      gen_rtx_raw_REG (SImode, CRIS_SRP_REGNUM));

      emit_jump_insn (gen_indirect_jump (gen_rtx_raw_REG (SImode, 13)));
    }
  else if (TARGET_V32)
    emit_jump_insn (gen_cris_return_v32 (gen_rtx_raw_REG (SImode,
							  CRIS_SRP_REGNUM)));
  else
    emit_jump_insn (gen_rtx_RETURN (VOIDmode));
}

/* Worker function for generating movem from mem for load_multiple.  */

rtx
gen_crisv32_movem_load (osrc, nregs_rtx, nprefix)
     rtx osrc, nregs_rtx;
     int nprefix;
{
  int nregs = INTVAL (nregs_rtx);
  rtvec vec;
  int eltno = 1;
  int i;
  rtx srcreg = XEXP (osrc, 0);
  rtx src = osrc;

  if (GET_CODE (srcreg) == POST_INC)
    srcreg = XEXP (srcreg, 0);

  if (!REG_P (srcreg))
    abort ();

  /* Don't use movem for just one insn.  The insns are equivalent except
     for the pipeline hazard; movem does not forward the loaded
     registers so there's a three cycles penalty for use.  */
  if (nregs == 1)
    return gen_movsi (gen_rtx_REG (SImode, 0), osrc);

  vec = rtvec_alloc (nprefix + nregs
		     + (GET_CODE (XEXP (osrc, 0)) == POST_INC));
  src = replace_equiv_address (osrc, srcreg);
  RTVEC_ELT (vec, nprefix)
    = gen_rtx_SET (VOIDmode, gen_rtx_REG (SImode, 0), src);

  if (GET_CODE (XEXP (osrc, 0)) == POST_INC)
    {
      RTVEC_ELT (vec, nprefix + 1)
	= gen_rtx_SET (VOIDmode, srcreg, plus_constant (srcreg, nregs * 4));
      eltno++;
    }

  for (i = 1; i < nregs; i++, eltno++)
    RTVEC_ELT (vec, nprefix + eltno)
      = gen_rtx_SET (VOIDmode, gen_rtx_REG (SImode, i),
		     adjust_address_nv (src, SImode, i * 4));

  return gen_rtx_PARALLEL (VOIDmode, vec);
}

/* Worker function for generating movem to mem for store_multiple.  */

rtx
gen_crisv32_movem_store (dest, nregs_rtx, frame_related)
     rtx dest, nregs_rtx;
     int frame_related;
{
  int nregs = INTVAL (nregs_rtx);
  rtvec vec;
  int eltno = 1;
  int i;
  rtx destreg = XEXP (dest, 0);

  if (GET_CODE (destreg) == POST_INC)
    destreg = XEXP (destreg, 0);

  if (!REG_P (destreg))
    abort ();

  /* Don't use movem for just one insn.  The insns are equivalent except
     for the pipeline hazard; movem does not forward the loaded
     registers so there's a three cycles penalty for use.  */
  if (nregs == 1)
    return gen_movsi (dest, gen_rtx_REG (SImode, 0));

  vec = rtvec_alloc (nregs + (GET_CODE (XEXP (dest, 0)) == POST_INC));
  RTVEC_ELT (vec, 0)
    = gen_rtx_SET (VOIDmode, replace_equiv_address (dest, destreg),
		   gen_rtx_REG (SImode, 0));
  if (frame_related)
    RTX_FRAME_RELATED_P (RTVEC_ELT (vec, 0)) = 1;

  if (GET_CODE (XEXP (dest, 0)) == POST_INC)
    {
      RTVEC_ELT (vec, 1)
	= gen_rtx_SET (VOIDmode, destreg, plus_constant (destreg, nregs * 4));
      if (frame_related)
	RTX_FRAME_RELATED_P (RTVEC_ELT (vec, 1)) = 1;
      eltno++;
    }

  for (i = 1; i < nregs; i++, eltno++)
    {
      RTVEC_ELT (vec, eltno)
	= gen_rtx_SET (VOIDmode, adjust_address_nv (dest, SImode, i * 4),
		       gen_rtx_REG (SImode, i));
      if (frame_related)
	RTX_FRAME_RELATED_P (RTVEC_ELT (vec, eltno)) = 1;
    }

  return gen_rtx_PARALLEL (VOIDmode, vec);
}

/* Worker function for expanding the address for PIC function calls.  */

void
cris_expand_pic_call_address (opp)
     rtx *opp;
{
  rtx op = *opp;

  if (!MEM_P (op))
    abort ();

  op = XEXP (op, 0);

  /* It might be that code can be generated that jumps to 0 (or to a
     specific address).  Don't die on that.  (There is a
     testcase.)  */
  if (CONSTANT_ADDRESS_P (op) && GET_CODE (op) != CONST_INT)
    {
      enum cris_pic_symbol_type t = cris_pic_symbol_type_of (op);

      if (no_new_pseudos)
	abort ();

      /* For local symbols (non-PLT), just get the plain symbol
	 reference into a register.  For symbols that can be PLT, make
	 them PLT.  */
      if (t == cris_rel_symbol)
	{
	  /* For v32, we're fine as-is; just PICify the symbol.  Forcing
	     into a register caused performance regression for 3.2.1,
	     observable in __floatdidf and elsewhere in libgcc.  */
	  if (TARGET_V32)
	    {
	      rtx sym = GET_CODE (op) != CONST ? op : get_related_value (op);
	      HOST_WIDE_INT offs = get_integer_term (op);

	      /* We can't get calls to sym+N, N integer, can we?  */
	      if (get_integer_term (op) != 0)
		abort ();

	      op = gen_rtx_CONST (Pmode,
				  gen_rtx_UNSPEC (Pmode, gen_rtvec (1, sym),
						  CRIS_UNSPEC_PCREL));
	    }
	  else
	    op = force_reg (Pmode, op);
	}
      else if (t == cris_got_symbol)
	{
	  if (TARGET_AVOID_GOTPLT)
	    {
	      /* Change a "jsr sym" into (allocate register rM, rO)
		 "move.d (const (unspec [sym rPIC] CRIS_UNSPEC_PLT_GOTREL)),rM"
		 "add.d rPIC,rM,rO", "jsr rO" for pre-v32 and
		 "jsr (const (unspec [sym rPIC] CRIS_UNSPEC_PLT_PCREL))"
		 for v32.  */
	      rtx tem, rm, ro;

	      if (no_new_pseudos)
		abort ();

	      current_function_uses_pic_offset_table = 1;
	      tem = gen_rtx_UNSPEC (Pmode,
				    gen_rtvec (2, op, pic_offset_table_rtx),
				    TARGET_V32
				    ? CRIS_UNSPEC_PLT_PCREL
				    : CRIS_UNSPEC_PLT_GOTREL);
	      tem = gen_rtx_CONST (Pmode, tem);
	      if (TARGET_V32)
		op = tem;
	      else
		{
		  rm = gen_reg_rtx (Pmode);
		  emit_move_insn (rm, tem);
		  ro = gen_reg_rtx (Pmode);
		  if (emit_insn (gen_addsi3 (ro, rm, pic_offset_table_rtx))
		      == NULL_RTX)
		    internal_error ("emit_insn failed in movsi got");
		  op = ro;
		}
	    }
	  else
	    {
	      /* Change a "jsr sym" into (allocate register rM, rO)
		 "move.d (const (unspec [sym] CRIS_UNSPEC_PLTGOTREAD)),rM"
		 "add.d rPIC,rM,rO" "jsr [rO]" with the memory access
		 marked as not trapping and not aliasing.  No "move.d
		 [rO],rP" as that would invite to re-use of a value
		 that should not be reused.  FIXME: Need a peephole2
		 for cases when this is cse:d from the call, to change
		 back to just get the PLT entry address, so we don't
		 resolve the same symbol over and over (the memory
		 access of the PLTGOT isn't constant).  */
	      rtx tem, mem, rm, ro;

	      if (no_new_pseudos)
		abort ();

	      current_function_uses_pic_offset_table = 1;
	      tem = gen_rtx_UNSPEC (Pmode,
				    gen_rtvec (2, op, pic_offset_table_rtx),
				    CRIS_UNSPEC_PLTGOTREAD);
	      rm = gen_reg_rtx (Pmode);
	      emit_move_insn (rm, gen_rtx_CONST (Pmode, tem));
	      ro = gen_reg_rtx (Pmode);
	      if (emit_insn (gen_addsi3 (ro, rm, pic_offset_table_rtx))
		  == NULL_RTX)
		internal_error ("emit_insn failed in movsi got");
	      mem = gen_rtx_MEM (Pmode, ro);

	      /* This MEM doesn't alias anything.  Whether it aliases
		 other same symbols is unimportant.  */
	      set_mem_alias_set (mem, new_alias_set ());
	      MEM_NOTRAP_P (mem) = 1;
	      /* We don't mark it as readonly, because we don't want it
		 re-used over calls.  Or actually, not the value before
		 the first call not re-used after it; re-use would be
		 preferred for the third and following calls.  */
	      op = mem;
	    }
	}
      else
	/* Can't possibly get a GOT-needing-fixup for a function-call,
	   right?  */
	fatal_insn ("Unidentifiable call op", op);

      /* Don't use "replace_equiv_address (*opp, op)": that'll force op
	 argument to be a valid operand, which we don't want: it'll
	 cause performance regression for v32 (see above) and it's wrong
	 as this is a "call", which has the unfortunate definition of
	 the operand always being wrapped in mem though no memory
	 operation takes place (we don't count the instruction fetch)
	 and it isn't a valid general_operand anyway for PIC.
	 We shouldn't need to clear any memory attributes.  If there are
	 any at this time, that's a bug.  */
      XEXP (*opp, 0) = copy_rtx (op);
    }
}

/* Use from within code, from e.g. PRINT_OPERAND and
   PRINT_OPERAND_ADDRESS.  Macros used in output_addr_const need to emit
   different things depending on whether code operand or constant is
   emitted.  */

static void
cris_output_addr_const (file, x)
     FILE *file;
     rtx x;
{
   in_code++;
   output_addr_const (file, x);
   in_code--;
}

/* Worker function for ASM_OUTPUT_SYMBOL_REF.  */
  
void
cris_asm_output_symbol_ref (file, x)
     FILE *file;
     rtx x;
{
  if (flag_pic && in_code > 0)
    {
      const char *origstr = XSTR (x, 0);
      const char *str;
      STRIP_NAME_ENCODING (str, origstr);
      assemble_name (file, str);
    }
  else
    assemble_name (file, XSTR (x, 0));
}

/* Worker function for ASM_OUTPUT_LABEL_REF.  */

void
cris_asm_output_label_ref (file, buf)
     FILE *file;
     char *buf;
{
  assemble_name (file, buf);
}

/* Worker function for OUTPUT_ADDR_CONST_EXTRA.  */
  
bool
cris_output_addr_const_extra (file, xconst)
     FILE *file;
     rtx xconst;
{
  switch (GET_CODE (xconst))
    {
      rtx x;

    case UNSPEC:
      x = XVECEXP (xconst, 0, 0);
      if (!(GET_CODE (x) == SYMBOL_REF
	    || GET_CODE (x) == LABEL_REF
	    || GET_CODE (x) == CONST))
	abort ();

      output_addr_const (file, x);
      switch (XINT (xconst, 1))
	{
	case CRIS_UNSPEC_PLT_PCREL:
	  if (!TARGET_V32)
	    abort ();
	  fprintf (file, ":PLT");
	  break;

	case CRIS_UNSPEC_PLT_GOTREL:
	  if (TARGET_V32)
	    abort ();
	  fprintf (file, ":PLTG");
	  break;

	case CRIS_UNSPEC_GOTREL:
	  if (TARGET_V32)
	    abort ();
	  fprintf (file, ":GOTOFF");
	  break;

	case CRIS_UNSPEC_PCREL:
	  /* Accessed with LAPC or BSR; no separate suffix.  */
	  if (!TARGET_V32)
	    abort ();
	  break;

	case CRIS_UNSPEC_GOTREAD:
 	  if (flag_pic == 1)
	    fprintf (file, ":GOT16");
 	  else
	    fprintf (file, ":GOT");
	  break;
	      
	case CRIS_UNSPEC_PLTGOTREAD:
	  if (flag_pic == 1)
	    fprintf (file, CRIS_GOTPLT_SUFFIX "16");
	  else
	    fprintf (file, CRIS_GOTPLT_SUFFIX);
	  break;

	default:
	  abort ();
	}
      return true;

    default:
      return false;
    }
}

/* The ENCODE_SECTION_INFO worker.  Code-in whether we can get away
   without a GOT entry (needed for externally visible objects but not for
   functions) into SYMBOL_REF_FLAG and add the PLT suffix for global
   functions.  */

void
cris_encode_section_info (exp)
     tree exp;
{
  if (flag_pic)
    {
      if (DECL_P (exp))
	{
	  if (TREE_CODE (exp) == FUNCTION_DECL
	      && (TREE_PUBLIC (exp) || DECL_WEAK (exp)))
	    SYMBOL_REF_FLAG (XEXP (DECL_RTL (exp), 0)) = 0;
	  else
	    SYMBOL_REF_FLAG (XEXP (DECL_RTL (exp), 0))
	      = ! TREE_PUBLIC (exp) && ! DECL_WEAK (exp);
	}
      else
	/* Others are local entities.  */
	SYMBOL_REF_FLAG (XEXP (TREE_CST_RTL (exp), 0)) = 1;
    }
}

/* We want an alias set for use in storing and reading from the saved
   register set, and we want it to be separate from the "frame" alias
   set.  FIXME: Should this be per-function?  */

HOST_WIDE_INT
cris_get_sr_alias_set ()
{
  static HOST_WIDE_INT set = -1;

  if (set == -1)
    set = new_alias_set ();

  return set;
}

/* We need to keep the register where we stored the initial-srp value
   despite the non-exceptional lifetime being over, lest it gets
   overwritten before used by EH.  */

int
cris_epilogue_eh_uses (regno)
     int regno;
{
  /* GCC will sometimes try to remove the register-save of the return
     address, when only noreturn functions are called
     (libgcc2.c:__absvsi2).  Say the register is really used.  */
  return (cfun->machine->srp_save_register != NULL)
    ? regno == (int) REGNO (cfun->machine->srp_save_register) : 0;
}

/* Make sure operands are in the right order for an addsi3 insn as
   generated by a define_split.  A MEM as the first operand isn't
   recognized by addsi3 after reload.  OPERANDS contains the operands,
   with the first at OPERANDS[N] and the second at OPERANDS[N+1].  */

void
cris_order_for_addsi3 (operands, n)
     rtx *operands;
     int n;
{
  if (MEM_P (operands[n]))
    {
      rtx tem = operands[n];
      operands[n] = operands[n + 1];
      operands[n + 1] = tem;
    }
}

#if 0
/* Various small functions to replace macros.  Only called from a
   debugger.  They might collide with gcc functions or system functions,
   so only emit them when '#if 1' above.  */

enum rtx_code Get_code PARAMS ((rtx));

enum rtx_code
Get_code (x)
     rtx x;
{
  return GET_CODE (x);
}

const char *Get_mode PARAMS ((rtx));

const char *
Get_mode (x)
     rtx x;
{
  return GET_MODE_NAME (GET_MODE (x));
}

rtx Xexp PARAMS ((rtx, int));

rtx
Xexp (x, n)
     rtx x;
     int n;
{
  return XEXP (x, n);
}

rtx Xvecexp PARAMS ((rtx, int, int));

rtx
Xvecexp (x, n, m)
     rtx x;
     int n;
     int m;
{
  return XVECEXP (x, n, m);
}

int Get_rtx_len PARAMS ((rtx));

int
Get_rtx_len (x)
     rtx x;
{
  return GET_RTX_LENGTH (GET_CODE (x));
}

/* Use upper-case to distinguish from local variables that are sometimes
   called next_insn and prev_insn.  */

rtx Next_insn PARAMS ((rtx));

rtx
Next_insn (insn)
     rtx insn;
{
  return NEXT_INSN (insn);
}

rtx Prev_insn PARAMS ((rtx));

rtx
Prev_insn (insn)
     rtx insn;
{
  return PREV_INSN (insn);
}
#endif

/*
 * Local variables:
 * eval: (c-set-style "gnu")
 * indent-tabs-mode: t
 * End:
 */
