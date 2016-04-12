#ifndef _ASM_CRIS_IO_H
#define _ASM_CRIS_IO_H

#include <asm/page.h>   /* for __va, __pa */
#include <asm/arch/io.h>

/*
 * Change virtual addresses to physical addresses and vv.
 */

extern inline unsigned long virt_to_phys(volatile void * address)
{
	return __pa(address);
}

extern inline void * phys_to_virt(unsigned long address)
{
	return __va(address);
}

extern void __iomem * __ioremap(unsigned long offset, unsigned long size, unsigned long flags);

extern inline void __iomem * ioremap (unsigned long offset, unsigned long size)
{
	return __ioremap(offset, size, 0);
}

extern void iounmap(volatile void * __iomem addr);

/*
 * IO bus memory addresses are also 1:1 with the physical address
 */
#define virt_to_bus virt_to_phys
#define bus_to_virt phys_to_virt

/*
 * readX/writeX() are used to access memory mapped devices. On some
 * architectures the memory mapped IO stuff needs to be accessed
 * differently. On the CRIS architecture, we just read/write the
 * memory location directly.
 */
static inline unsigned char readb(const volatile void __iomem *addr)
{
	return *(volatile unsigned char __force *) addr;
}
static inline unsigned short readw(const volatile void __iomem *addr)
{
	return *(volatile unsigned short __force *) addr;
}
static inline unsigned int readl(const volatile void __iomem *addr)
{
	return *(volatile unsigned int __force *) addr;
}
#define readb_relaxed(addr) readb(addr)
#define readw_relaxed(addr) readw(addr)
#define readl_relaxed(addr) readl(addr)
#define __raw_readb readb
#define __raw_readw readw
#define __raw_readl readl

static inline void writeb(unsigned char b, volatile void __iomem *addr)
{
	*(volatile unsigned char __force *) addr = b;
}
static inline void writew(unsigned short b, volatile void __iomem *addr)
{
	*(volatile unsigned short __force *) addr = b;
}
static inline void writel(unsigned int b, volatile void __iomem *addr)
{
	*(volatile unsigned int __force *) addr = b;
}
#define __raw_writeb writeb
#define __raw_writew writew
#define __raw_writel writel

static inline void memset_io(volatile void __iomem *addr, unsigned char val, int count)
{
	memset((void __force *) addr, val, count);
}
static inline void memcpy_fromio(void *dst, volatile void __iomem *src, int count)
{
	memcpy(dst, (void __force *) src, count);
}
static inline void memcpy_toio(volatile void __iomem *dst, const void *src, int count)
{
	memcpy((void __force *) dst, src, count);
}

/*
 * Again, CRIS does not require mem IO specific function.
 */

#define eth_io_copy_and_sum(a,b,c,d)	eth_copy_and_sum((a),(void __force *)(b),(c),(d))

/* The following is junk needed for the arch-independent code but which
 * we never use in the CRIS port
 */

#define IO_SPACE_LIMIT 0xffff
#define inb(x) (0)
#define inw(x) (0)
#define inl(x) (0)
#define outb(x,y)
#define outw(x,y)
#define outl(x,y)
#define insb(x,y,z)
#define insw(x,y,z)
#define insl(x,y,z)
#define outsb(x,y,z)
#define outsw(x,y,z)
#define outsl(x,y,z)

#endif
