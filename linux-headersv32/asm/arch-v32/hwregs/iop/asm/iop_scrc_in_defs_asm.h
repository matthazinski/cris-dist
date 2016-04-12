#ifndef __iop_scrc_in_defs_asm_h
#define __iop_scrc_in_defs_asm_h

/*
 * This file is autogenerated from
 *   file:           ../../inst/io_proc/rtl/iop_scrc_in.r
 *     id:           iop_scrc_in.r,v 1.5 2004/01/09 07:39:59 jonaso Exp 
 *     last modfied: Tue Mar 30 22:27:01 2004
 * 
 *   by /n/asic/design/tools/rdesc/src/rdes2c -asm --outfile asm/iop_scrc_in_defs_asm.h ../../inst/io_proc/rtl/iop_scrc_in.r
 *      id: $Id: iop_scrc_in_defs_asm.h,v 1.4 2004/05/12 11:43:18 larsv Exp $
 * Any changes here will be lost.
 *
 * -*- buffer-read-only: t -*-
 */

#ifndef REG_FIELD
#define REG_FIELD( scope, reg, field, value ) \
  REG_FIELD_X_( value, reg_##scope##_##reg##___##field##___lsb )
#define REG_FIELD_X_( value, shift ) ((value) << shift)
#endif

#ifndef REG_STATE
#define REG_STATE( scope, reg, field, symbolic_value ) \
  REG_STATE_X_( regk_##scope##_##symbolic_value, reg_##scope##_##reg##___##field##___lsb )
#define REG_STATE_X_( k, shift ) (k << shift)
#endif

#ifndef REG_MASK
#define REG_MASK( scope, reg, field ) \
  REG_MASK_X_( reg_##scope##_##reg##___##field##___width, reg_##scope##_##reg##___##field##___lsb )
#define REG_MASK_X_( width, lsb ) (((1 << width)-1) << lsb)
#endif

#ifndef REG_LSB
#define REG_LSB( scope, reg, field ) reg_##scope##_##reg##___##field##___lsb
#endif

#ifndef REG_BIT
#define REG_BIT( scope, reg, field ) reg_##scope##_##reg##___##field##___bit
#endif

#ifndef REG_ADDR
#define REG_ADDR( scope, inst, reg ) REG_ADDR_X_(inst, reg_##scope##_##reg##_offset)
#define REG_ADDR_X_( inst, offs ) ((inst) + offs)
#endif

#ifndef REG_ADDR_VECT
#define REG_ADDR_VECT( scope, inst, reg, index ) \
         REG_ADDR_VECT_X_(inst, reg_##scope##_##reg##_offset, index, \
			 STRIDE_##scope##_##reg )
#define REG_ADDR_VECT_X_( inst, offs, index, stride ) \
                          ((inst) + offs + (index) * stride)
#endif

/* Register rw_cfg, scope iop_scrc_in, type rw */
#define reg_iop_scrc_in_rw_cfg___trig___lsb 0
#define reg_iop_scrc_in_rw_cfg___trig___width 2
#define reg_iop_scrc_in_rw_cfg_offset 0

/* Register rw_ctrl, scope iop_scrc_in, type rw */
#define reg_iop_scrc_in_rw_ctrl___dif_in_en___lsb 0
#define reg_iop_scrc_in_rw_ctrl___dif_in_en___width 1
#define reg_iop_scrc_in_rw_ctrl___dif_in_en___bit 0
#define reg_iop_scrc_in_rw_ctrl_offset 4

/* Register r_stat, scope iop_scrc_in, type r */
#define reg_iop_scrc_in_r_stat___error___lsb 0
#define reg_iop_scrc_in_r_stat___error___width 1
#define reg_iop_scrc_in_r_stat___error___bit 0
#define reg_iop_scrc_in_r_stat_offset 8

/* Register rw_initial_crc, scope iop_scrc_in, type rw */
#define reg_iop_scrc_in_rw_initial_crc_offset 12

/* Register rs_computed_crc, scope iop_scrc_in, type rs */
#define reg_iop_scrc_in_rs_computed_crc_offset 16

/* Register r_computed_crc, scope iop_scrc_in, type r */
#define reg_iop_scrc_in_r_computed_crc_offset 20

/* Register rw_crc, scope iop_scrc_in, type rw */
#define reg_iop_scrc_in_rw_crc_offset 24

/* Register rw_correct_crc, scope iop_scrc_in, type rw */
#define reg_iop_scrc_in_rw_correct_crc_offset 28

/* Register rw_wr1bit, scope iop_scrc_in, type rw */
#define reg_iop_scrc_in_rw_wr1bit___data___lsb 0
#define reg_iop_scrc_in_rw_wr1bit___data___width 2
#define reg_iop_scrc_in_rw_wr1bit___last___lsb 2
#define reg_iop_scrc_in_rw_wr1bit___last___width 2
#define reg_iop_scrc_in_rw_wr1bit_offset 32


/* Constants */
#define regk_iop_scrc_in_dif_in                   0x00000002
#define regk_iop_scrc_in_hi                       0x00000000
#define regk_iop_scrc_in_neg                      0x00000002
#define regk_iop_scrc_in_no                       0x00000000
#define regk_iop_scrc_in_pos                      0x00000001
#define regk_iop_scrc_in_pos_neg                  0x00000003
#define regk_iop_scrc_in_r_computed_crc_default   0x00000000
#define regk_iop_scrc_in_rs_computed_crc_default  0x00000000
#define regk_iop_scrc_in_rw_cfg_default           0x00000000
#define regk_iop_scrc_in_rw_ctrl_default          0x00000000
#define regk_iop_scrc_in_rw_initial_crc_default   0x00000000
#define regk_iop_scrc_in_set0                     0x00000000
#define regk_iop_scrc_in_set1                     0x00000001
#define regk_iop_scrc_in_yes                      0x00000001
#endif /* __iop_scrc_in_defs_asm_h */
