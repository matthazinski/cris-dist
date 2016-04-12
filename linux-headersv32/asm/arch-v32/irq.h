#ifndef _ASM_ARCH_IRQ_H
#define _ASM_ARCH_IRQ_H

#include <linux/config.h>
#include "hwregs/intr_vect.h"

/* Number of non-cpu interrupts. */
#define NR_IRQS 0x20
#define FIRST_IRQ 0x31

/* 
 * Ugly define. Make _absolutely_ sure this matches regi_irq in
 * hwregs/reg_map_asm.h.
 *
 * We need to define this here to avoid clashes between C and assembler include
 * files.
 */
#define IRQ_REG_ADDR	0xb001c000

#ifndef __ASSEMBLY__
/* Global IRQ vector. */
typedef void (*irqvectptr)(void);

struct etrax_interrupt_vector {
	irqvectptr v[256];
};

extern struct etrax_interrupt_vector *etrax_irv;	/* head.S */

void mask_irq(int irq);
void unmask_irq(int irq);
void set_exception_vector(int n, irqvectptr addr);

/* Save registers so that they match pt_regs. */
#define SAVE_ALL \
	"subq 12,$sp\n\t"	\
	"move $erp,[$sp]\n\t"	\
	"subq 4,$sp\n\t"	\
	"move $srp,[$sp]\n\t"	\
	"subq 4,$sp\n\t"	\
	"move $ccs,[$sp]\n\t"	\
	"subq 4,$sp\n\t"	\
	"move $spc,[$sp]\n\t"	\
	"subq 4,$sp\n\t"	\
	"move $mof,[$sp]\n\t"	\
	"subq 4,$sp\n\t"	\
	"move $srs,[$sp]\n\t"	\
	"subq 4,$sp\n\t"	\
	"move.d $acr,[$sp]\n\t"	\
	"subq 14*4,$sp\n\t"	\
	"movem $r13,[$sp]\n\t"	\
	"subq 4,$sp\n\t"	\
	"move.d $r10,[$sp]\n"

#define STR2(x) #x
#define STR(x) STR2(x)

#define BLOCK_IRQ(mask) 		\
	"move.d " STR(IRQ_REG_ADDR) ",$acr\n\t"	\
	"move.d [$acr],$r0\n\t"		\
	"and.d ~" #mask ",$r0\n\t"	\
	"move.d $r0,[$acr]\n\t"

#define UNBLOCK_IRQ(mask)		\
	"move.d " STR(IRQ_REG_ADDR) ",$acr\n\t"	\
	"move.d [$acr],$r0\n\t"		\
	"or.d " #mask ",$r0\n\t"	\
	"move.d $r0,[$acr]\n\t"

#define IRQ_NAME2(nr) nr##_interrupt(void)
#define IRQ_NAME(nr) IRQ_NAME2(IRQ##nr)
#define sIRQ_NAME(nr) IRQ_NAME2(sIRQ##nr)
#define BAD_IRQ_NAME(nr) IRQ_NAME2(bad_IRQ##nr)

/* 
 * The reason for setting the S-bit when debugging the kernel is that we want
 * hardware breakpoints to remain active while we are in an exception handler.
 * Note that we cannot simply copy S1, since we may come here from user-space,
 * or any context where the S-bit wasn't set.
 */
#ifdef CONFIG_ETRAX_KGDB
#define KGDB_FIXUP \
	"move $ccs, $r10\n\t"		\
	"or.d (1<<9), $r10\n\t"		\
	"move $r10, $ccs\n\t"
#else
#define KGDB_FIXUP ""
#endif

/*
 * Make sure the causing IRQ is blocked, then call do_IRQ. After that, unblock
 * and jump to ret_from_intr which is found in entry.S.
 *
 * The reason for blocking the IRQ is to allow an sti() before the handler,
 * which will acknowledge the interrupt, is run.
 */
#define BUILD_IRQ(nr, mask)		\
void IRQ_NAME(nr);			\
void sIRQ_NAME(nr);			\
void BAD_IRQ_NAME(nr);			\
__asm__ (				\
	".text\n\t"			\
	"IRQ" #nr "_interrupt:\n\t" 	\
	SAVE_ALL			\
	"sIRQ" #nr "_interrupt:\n\t" 	\
	BLOCK_IRQ(mask)			\
	KGDB_FIXUP                      \
	"move.d "#nr",$r10\n\t"		\
	"move.d $sp,$r11\n\t"		\
	"jsr do_IRQ\n\t"		\
	"nop\n\t"			\
	UNBLOCK_IRQ(mask)		\
	"jump ret_from_intr\n\t"	\
	"nop\n\t"			\
	"bad_IRQ" #nr "_interrupt:\n\t" \
	"subq 4,$sp\n\t"		\
	"move.d $r0,[$sp]\n\t"		\
	BLOCK_IRQ(mask)			\
	"move.d [$sp+],$r0\n\t"		\
	"rete\n\t"			\
	"rfe\n");

/* 
 * This is subtle. The timer interrupt is crucial and it should not be disabled
 * for too long. However, if it had been a normal interrupt as per BUILD_IRQ, it
 * would have been BLOCK'ed, and then softirq's are run before we return here to
 * UNBLOCK. If the softirq's take too much time to run, the timer irq won't run
 * and the watchdog will kill us.
 *
 * Furthermore, if a lot of other irq's occur before we return here, the
 * multiple_irq handler is run and it prioritizes the timer interrupt. However
 * if we had BLOCK'edit here, we would not get the multiple_irq at all.
 *
 * The non-blocking here is based on the knowledge that the timer interrupt is
 * registred as a fast interrupt (SA_INTERRUPT) so that we _know_ there will not
 * be an sti() before the timer irq handler is run to acknowledge the interrupt.
 */
#define BUILD_TIMER_IRQ(nr, mask) 	\
void IRQ_NAME(nr);			\
void sIRQ_NAME(nr);			\
void BAD_IRQ_NAME(nr);			\
__asm__ (				\
	".text\n\t"			\
	"IRQ" #nr "_interrupt:\n\t"	\
	SAVE_ALL			\
	"sIRQ" #nr "_interrupt:\n\t"	\
        KGDB_FIXUP                      \
	"move.d "#nr",$r10\n\t"		\
	"move.d $sp,$r11\n\t"		\
	"jsr do_IRQ\n\t"		\
	"nop\n\t"			\
	"jump ret_from_intr\n\t"	\
	"nop\n\t"			\
	"bad_IRQ" #nr "_interrupt:\n\t" \
	"subq 4,$sp\n\t"		\
	"move.d $r0,[$sp]\n\t"		\
	BLOCK_IRQ(mask)			\
	"move.d [$sp+],$r0\n\t"		\
	"rete\n\t"			\
	"rfe\n");

#endif /* __ASSEMBLY__ */
#endif /* _ASM_ARCH_IRQ_H */
