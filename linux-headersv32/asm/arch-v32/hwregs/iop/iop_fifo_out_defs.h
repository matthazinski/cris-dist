#ifndef __iop_fifo_out_defs_h
#define __iop_fifo_out_defs_h

/*
 * This file is autogenerated from
 *   file:           ../../inst/io_proc/rtl/iop_fifo_out.r
 *     id:           <not found>
 *     last modfied: Tue Mar 30 22:28:38 2004
 * 
 *   by /n/asic/design/tools/rdesc/src/rdes2c --outfile iop_fifo_out_defs.h ../../inst/io_proc/rtl/iop_fifo_out.r
 *      id: $Id: iop_fifo_out_defs.h,v 1.3 2004/05/07 12:54:31 larsv Exp $
 * Any changes here will be lost.
 *
 * -*- buffer-read-only: t -*-
 */
/* Main access macros */
#ifndef REG_RD
#define REG_RD( scope, inst, reg ) \
  REG_READ( reg_##scope##_##reg, \
            (inst) + REG_RD_ADDR_##scope##_##reg )
#endif

#ifndef REG_WR
#define REG_WR( scope, inst, reg, val ) \
  REG_WRITE( reg_##scope##_##reg, \
             (inst) + REG_WR_ADDR_##scope##_##reg, (val) )
#endif

#ifndef REG_RD_VECT
#define REG_RD_VECT( scope, inst, reg, index ) \
  REG_READ( reg_##scope##_##reg, \
            (inst) + REG_RD_ADDR_##scope##_##reg + \
	    (index) * STRIDE_##scope##_##reg )
#endif

#ifndef REG_WR_VECT
#define REG_WR_VECT( scope, inst, reg, index, val ) \
  REG_WRITE( reg_##scope##_##reg, \
             (inst) + REG_WR_ADDR_##scope##_##reg + \
	     (index) * STRIDE_##scope##_##reg, (val) )
#endif

#ifndef REG_RD_INT
#define REG_RD_INT( scope, inst, reg ) \
  REG_READ( int, (inst) + REG_RD_ADDR_##scope##_##reg )
#endif

#ifndef REG_WR_INT
#define REG_WR_INT( scope, inst, reg, val ) \
  REG_WRITE( int, (inst) + REG_WR_ADDR_##scope##_##reg, (val) )
#endif

#ifndef REG_RD_INT_VECT
#define REG_RD_INT_VECT( scope, inst, reg, index ) \
  REG_READ( int, (inst) + REG_RD_ADDR_##scope##_##reg + \
	    (index) * STRIDE_##scope##_##reg )
#endif

#ifndef REG_WR_INT_VECT
#define REG_WR_INT_VECT( scope, inst, reg, index, val ) \
  REG_WRITE( int, (inst) + REG_WR_ADDR_##scope##_##reg + \
	     (index) * STRIDE_##scope##_##reg, (val) )
#endif

#ifndef REG_TYPE_CONV
#define REG_TYPE_CONV( type, orgtype, val ) \
  ( { union { orgtype o; type n; } r; r.o = val; r.n; } )
#endif

#ifndef reg_page_size
#define reg_page_size 8192
#endif

#ifndef REG_ADDR
#define REG_ADDR( scope, inst, reg ) \
  ( (inst) + REG_RD_ADDR_##scope##_##reg )
#endif

#ifndef REG_ADDR_VECT
#define REG_ADDR_VECT( scope, inst, reg, index ) \
  ( (inst) + REG_RD_ADDR_##scope##_##reg + \
    (index) * STRIDE_##scope##_##reg )
#endif

/* C-code for register scope iop_fifo_out */

/* Register rw_cfg, scope iop_fifo_out, type rw */
typedef struct {
  unsigned int free_lim         : 3;
  unsigned int byte_order       : 2;
  unsigned int trig             : 2;
  unsigned int last_dis_dif_in  : 1;
  unsigned int mode             : 2;
  unsigned int delay_out_last   : 1;
  unsigned int last_dis_dif_out : 1;
  unsigned int dummy1           : 20;
} reg_iop_fifo_out_rw_cfg;
#define REG_RD_ADDR_iop_fifo_out_rw_cfg 0
#define REG_WR_ADDR_iop_fifo_out_rw_cfg 0

/* Register rw_ctrl, scope iop_fifo_out, type rw */
typedef struct {
  unsigned int dif_in_en  : 1;
  unsigned int dif_out_en : 1;
  unsigned int dummy1     : 30;
} reg_iop_fifo_out_rw_ctrl;
#define REG_RD_ADDR_iop_fifo_out_rw_ctrl 4
#define REG_WR_ADDR_iop_fifo_out_rw_ctrl 4

/* Register r_stat, scope iop_fifo_out, type r */
typedef struct {
  unsigned int avail_bytes    : 4;
  unsigned int last           : 8;
  unsigned int dif_in_en      : 1;
  unsigned int dif_out_en     : 1;
  unsigned int zero_data_last : 1;
  unsigned int dummy1         : 17;
} reg_iop_fifo_out_r_stat;
#define REG_RD_ADDR_iop_fifo_out_r_stat 8

/* Register rw_wr1byte, scope iop_fifo_out, type rw */
typedef struct {
  unsigned int data : 8;
  unsigned int dummy1 : 24;
} reg_iop_fifo_out_rw_wr1byte;
#define REG_RD_ADDR_iop_fifo_out_rw_wr1byte 12
#define REG_WR_ADDR_iop_fifo_out_rw_wr1byte 12

/* Register rw_wr2byte, scope iop_fifo_out, type rw */
typedef struct {
  unsigned int data : 16;
  unsigned int dummy1 : 16;
} reg_iop_fifo_out_rw_wr2byte;
#define REG_RD_ADDR_iop_fifo_out_rw_wr2byte 16
#define REG_WR_ADDR_iop_fifo_out_rw_wr2byte 16

/* Register rw_wr3byte, scope iop_fifo_out, type rw */
typedef struct {
  unsigned int data : 24;
  unsigned int dummy1 : 8;
} reg_iop_fifo_out_rw_wr3byte;
#define REG_RD_ADDR_iop_fifo_out_rw_wr3byte 20
#define REG_WR_ADDR_iop_fifo_out_rw_wr3byte 20

/* Register rw_wr4byte, scope iop_fifo_out, type rw */
typedef struct {
  unsigned int data : 32;
} reg_iop_fifo_out_rw_wr4byte;
#define REG_RD_ADDR_iop_fifo_out_rw_wr4byte 24
#define REG_WR_ADDR_iop_fifo_out_rw_wr4byte 24

/* Register rw_wr1byte_last, scope iop_fifo_out, type rw */
typedef struct {
  unsigned int data : 8;
  unsigned int dummy1 : 24;
} reg_iop_fifo_out_rw_wr1byte_last;
#define REG_RD_ADDR_iop_fifo_out_rw_wr1byte_last 28
#define REG_WR_ADDR_iop_fifo_out_rw_wr1byte_last 28

/* Register rw_wr2byte_last, scope iop_fifo_out, type rw */
typedef struct {
  unsigned int data : 16;
  unsigned int dummy1 : 16;
} reg_iop_fifo_out_rw_wr2byte_last;
#define REG_RD_ADDR_iop_fifo_out_rw_wr2byte_last 32
#define REG_WR_ADDR_iop_fifo_out_rw_wr2byte_last 32

/* Register rw_wr3byte_last, scope iop_fifo_out, type rw */
typedef struct {
  unsigned int data : 24;
  unsigned int dummy1 : 8;
} reg_iop_fifo_out_rw_wr3byte_last;
#define REG_RD_ADDR_iop_fifo_out_rw_wr3byte_last 36
#define REG_WR_ADDR_iop_fifo_out_rw_wr3byte_last 36

/* Register rw_wr4byte_last, scope iop_fifo_out, type rw */
typedef struct {
  unsigned int data : 32;
} reg_iop_fifo_out_rw_wr4byte_last;
#define REG_RD_ADDR_iop_fifo_out_rw_wr4byte_last 40
#define REG_WR_ADDR_iop_fifo_out_rw_wr4byte_last 40

/* Register rw_set_last, scope iop_fifo_out, type rw */
typedef unsigned int reg_iop_fifo_out_rw_set_last;
#define REG_RD_ADDR_iop_fifo_out_rw_set_last 44
#define REG_WR_ADDR_iop_fifo_out_rw_set_last 44

/* Register rs_rd_data, scope iop_fifo_out, type rs */
typedef unsigned int reg_iop_fifo_out_rs_rd_data;
#define REG_RD_ADDR_iop_fifo_out_rs_rd_data 48

/* Register r_rd_data, scope iop_fifo_out, type r */
typedef unsigned int reg_iop_fifo_out_r_rd_data;
#define REG_RD_ADDR_iop_fifo_out_r_rd_data 52

/* Register rw_strb_dif_out, scope iop_fifo_out, type rw */
typedef unsigned int reg_iop_fifo_out_rw_strb_dif_out;
#define REG_RD_ADDR_iop_fifo_out_rw_strb_dif_out 56
#define REG_WR_ADDR_iop_fifo_out_rw_strb_dif_out 56

/* Register rw_intr_mask, scope iop_fifo_out, type rw */
typedef struct {
  unsigned int urun      : 1;
  unsigned int last_data : 1;
  unsigned int dav       : 1;
  unsigned int free      : 1;
  unsigned int orun      : 1;
  unsigned int dummy1    : 27;
} reg_iop_fifo_out_rw_intr_mask;
#define REG_RD_ADDR_iop_fifo_out_rw_intr_mask 60
#define REG_WR_ADDR_iop_fifo_out_rw_intr_mask 60

/* Register rw_ack_intr, scope iop_fifo_out, type rw */
typedef struct {
  unsigned int urun      : 1;
  unsigned int last_data : 1;
  unsigned int dav       : 1;
  unsigned int free      : 1;
  unsigned int orun      : 1;
  unsigned int dummy1    : 27;
} reg_iop_fifo_out_rw_ack_intr;
#define REG_RD_ADDR_iop_fifo_out_rw_ack_intr 64
#define REG_WR_ADDR_iop_fifo_out_rw_ack_intr 64

/* Register r_intr, scope iop_fifo_out, type r */
typedef struct {
  unsigned int urun      : 1;
  unsigned int last_data : 1;
  unsigned int dav       : 1;
  unsigned int free      : 1;
  unsigned int orun      : 1;
  unsigned int dummy1    : 27;
} reg_iop_fifo_out_r_intr;
#define REG_RD_ADDR_iop_fifo_out_r_intr 68

/* Register r_masked_intr, scope iop_fifo_out, type r */
typedef struct {
  unsigned int urun      : 1;
  unsigned int last_data : 1;
  unsigned int dav       : 1;
  unsigned int free      : 1;
  unsigned int orun      : 1;
  unsigned int dummy1    : 27;
} reg_iop_fifo_out_r_masked_intr;
#define REG_RD_ADDR_iop_fifo_out_r_masked_intr 72


/* Constants */
enum {
  regk_iop_fifo_out_hi                     = 0x00000000,
  regk_iop_fifo_out_neg                    = 0x00000002,
  regk_iop_fifo_out_no                     = 0x00000000,
  regk_iop_fifo_out_off                    = 0x00000000,
  regk_iop_fifo_out_on                     = 0x00000001,
  regk_iop_fifo_out_order16                = 0x00000001,
  regk_iop_fifo_out_order24                = 0x00000002,
  regk_iop_fifo_out_order32                = 0x00000003,
  regk_iop_fifo_out_order8                 = 0x00000000,
  regk_iop_fifo_out_pos                    = 0x00000001,
  regk_iop_fifo_out_pos_neg                = 0x00000003,
  regk_iop_fifo_out_rw_cfg_default         = 0x00000024,
  regk_iop_fifo_out_rw_ctrl_default        = 0x00000000,
  regk_iop_fifo_out_rw_intr_mask_default   = 0x00000000,
  regk_iop_fifo_out_rw_set_last_default    = 0x00000000,
  regk_iop_fifo_out_rw_strb_dif_out_default = 0x00000000,
  regk_iop_fifo_out_rw_wr1byte_default     = 0x00000000,
  regk_iop_fifo_out_rw_wr1byte_last_default = 0x00000000,
  regk_iop_fifo_out_rw_wr2byte_default     = 0x00000000,
  regk_iop_fifo_out_rw_wr2byte_last_default = 0x00000000,
  regk_iop_fifo_out_rw_wr3byte_default     = 0x00000000,
  regk_iop_fifo_out_rw_wr3byte_last_default = 0x00000000,
  regk_iop_fifo_out_rw_wr4byte_default     = 0x00000000,
  regk_iop_fifo_out_rw_wr4byte_last_default = 0x00000000,
  regk_iop_fifo_out_size16                 = 0x00000002,
  regk_iop_fifo_out_size24                 = 0x00000001,
  regk_iop_fifo_out_size32                 = 0x00000000,
  regk_iop_fifo_out_size8                  = 0x00000003,
  regk_iop_fifo_out_yes                    = 0x00000001
};
#endif /* __iop_fifo_out_defs_h */
