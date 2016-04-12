/*
 *  linux/include/asm-cris/ide.h
 *
 *  Copyright (C) 2000-2004  Axis Communications AB
 *
 *  Authors:    Bjorn Wesen, Mikael Starvik
 *
 */

/*
 *  This file contains the ETRAX FS specific IDE code.
 */

#ifndef __ASMCRIS_IDE_H
#define __ASMCRIS_IDE_H

#ifdef __KERNEL__

#include <asm/arch/hwregs/intr_vect.h>
#include <asm/arch/hwregs/ata_defs.h>
#include <asm/io.h>
#include <asm-generic/ide_iops.h>


/* ETRAX FS can support 4 IDE busses on the same pins (serialized) */

#define MAX_HWIFS	4

extern __inline__ int ide_default_irq(unsigned long base)
{
	/* all IDE busses share the same IRQ,
	 * this has the side-effect that ide-probe.c will cluster our 4 interfaces
	 * together in a hwgroup, and will serialize accesses. this is good, because
	 * we can't access more than one interface at the same time on ETRAX100.
	 */
	return ATA_INTR_VECT; 
}

extern __inline__ unsigned long ide_default_io_base(int index)
{
	reg_ata_rw_ctrl2 ctrl2 = {.sel = index};
	/* we have no real I/O base address per interface, since all go through the
	 * same register. but in a bitfield in that register, we have the i/f number.
	 * so we can use the io_base to remember that bitfield.
	 */
        ctrl2.sel = index;
        
	return REG_TYPE_CONV(unsigned long, reg_ata_rw_ctrl2, ctrl2);
}

/* this is called once for each interface, to setup the port addresses. data_port is the result
 * of the ide_default_io_base call above. ctrl_port will be 0, but that is don't care for us.
 */

extern __inline__ void ide_init_hwif_ports(hw_regs_t *hw, unsigned long data_port, unsigned long ctrl_port, int *irq)
{
	int i;
	reg_ata_rw_ctrl2 ctrl2 = {0};

	/* fill in ports for ATA addresses 0 to 7 */

	for (i = IDE_DATA_OFFSET; i <= IDE_STATUS_OFFSET; i++) {
		ctrl2.addr = i;
		ctrl2.cs0 = regk_ata_active;
		hw->io_ports[i] = data_port | 
			REG_TYPE_CONV(unsigned int, reg_ata_rw_ctrl2, ctrl2);
	}

	/* the IDE control register is at ATA address 6, with CS1 active instead of CS0 */
	ctrl2.addr = 6;
	ctrl2.cs1 = regk_ata_active;
	ctrl2.cs0 = regk_ata_inactive;
	hw->io_ports[IDE_CONTROL_OFFSET] = data_port |
		REG_TYPE_CONV(unsigned int, reg_ata_rw_ctrl2, ctrl2);

	/* whats this for ? */

	hw->io_ports[IDE_IRQ_OFFSET] = 0;
}

extern __inline__ void ide_init_default_hwifs(void)
{
	hw_regs_t hw;
	int index;

	for(index = 0; index < MAX_HWIFS; index++) {
		ide_init_hwif_ports(&hw, ide_default_io_base(index), 0, NULL);
		hw.irq = ide_default_irq(ide_default_io_base(index));
		ide_register_hw(&hw, NULL);
	}
}

/* some configuration options we don't need */

#undef SUPPORT_VLB_SYNC
#define SUPPORT_VLB_SYNC 0

#endif /* __KERNEL__ */

#endif /* __ASMCRIS_IDE_H */
