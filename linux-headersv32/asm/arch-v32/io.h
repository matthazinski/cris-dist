#ifndef _ASM_ARCH_CRIS_IO_H
#define _ASM_ARCH_CRIS_IO_H

#include <asm/arch/hwregs/reg_map.h>
#include <asm/arch/hwregs/reg_rdwr.h>
#include <asm/arch/hwregs/gio_defs.h>
#include <linux/config.h>

/* Etrax shadow registers - which live in 
 * arch/cris/arch-v32/kernel/shadows.c 
 */

extern unsigned long port_cse1_shadow;
extern unsigned long port_csp0_shadow;
extern unsigned long port_csp4_shadow;


extern volatile unsigned long *port_cse1_addr;
extern volatile unsigned long *port_csp0_addr;
extern volatile unsigned long *port_csp4_addr;

 /* macro for setting regs through a shadow - 
  * r = register name (like R_PORT_PA_DATA)
  * s = shadow name (like port_pa_data_shadow)
  * b = bit number
  * v = value (0 or 1)
  */
 
#define REG_SHADOW_SET(r,s,b,v) *r = s = (s & ~(1 << (b))) | ((v) << (b))


/* macro for controlling a bit in a gio register - 
 * r = register name (like rw_pa_data_out)
 * b = bit number
 * v = value (0 or 1)
 */

#define GIO_REG_BIT_SET(r,b,v) do { \
  unsigned long s; \
  s = REG_TYPE_CONV(unsigned long, reg_gio_##r , REG_RD(gio, regi_gio, r)); \
  s = (s & ~(1 << (b))) | ((v) << (b)); \
  REG_WR(gio, regi_gio, r, REG_TYPE_CONV(reg_gio_##r , unsigned long, s)); \
  } while(0)


/* The LED's on various Etrax-based products are set differently. */

#if defined(CONFIG_ETRAX_NO_LEDS) || defined(CONFIG_SVINTO_SIM)
#undef CONFIG_ETRAX_PA_LEDS
#undef CONFIG_ETRAX_PB_LEDS
#undef CONFIG_ETRAX_CSP0_LEDS
#define LED_NETWORK_SET_G(x)
#define LED_NETWORK_SET_R(x)
#define LED_ACTIVE_SET_G(x)
#define LED_ACTIVE_SET_R(x)
#define LED_DISK_WRITE(x)
#define LED_DISK_READ(x)
#endif

#if !defined(CONFIG_ETRAX_CSP0_LEDS)
#define LED_BIT_SET(x)
#define LED_BIT_CLR(x)
#endif

#define LED_OFF    0x00
#define LED_GREEN  0x01
#define LED_RED    0x02
#define LED_ORANGE (LED_GREEN | LED_RED)

#if CONFIG_ETRAX_LED1G == CONFIG_ETRAX_LED1R 
#define LED_NETWORK_SET(x)                          \
	do {                                        \
		LED_NETWORK_SET_G((x) & LED_GREEN); \
	} while (0)
#else
#define LED_NETWORK_SET(x)                          \
	do {                                        \
		LED_NETWORK_SET_G((x) & LED_GREEN); \
		LED_NETWORK_SET_R((x) & LED_RED);   \
	} while (0)
#endif
#if CONFIG_ETRAX_LED2G == CONFIG_ETRAX_LED2R 
#define LED_ACTIVE_SET(x)                           \
	do {                                        \
		LED_ACTIVE_SET_G((x) & LED_GREEN);  \
	} while (0)
#else
#define LED_ACTIVE_SET(x)                           \
	do {                                        \
		LED_ACTIVE_SET_G((x) & LED_GREEN);  \
		LED_ACTIVE_SET_R((x) & LED_RED);    \
	} while (0)
#endif

#ifdef CONFIG_ETRAX_PA_LEDS
#define LED_NETWORK_SET_G(x) \
         GIO_REG_BIT_SET(rw_pa_data_out, CONFIG_ETRAX_LED1G, !(x))
#define LED_NETWORK_SET_R(x) \
         GIO_REG_BIT_SET(rw_pa_data_out, CONFIG_ETRAX_LED1R, !(x))
#define LED_ACTIVE_SET_G(x) \
         GIO_REG_BIT_SET(rw_pa_data_out, CONFIG_ETRAX_LED2G, !(x))
#define LED_ACTIVE_SET_R(x) \
         GIO_REG_BIT_SET(rw_pa_data_out, CONFIG_ETRAX_LED2R, !(x))
#define LED_DISK_WRITE(x) \
         do{\
                GIO_REG_BIT_SET(rw_pa_data_out, CONFIG_ETRAX_LED3G, !(x));\
                GIO_REG_BIT_SET(rw_pa_data_out, CONFIG_ETRAX_LED3R, !(x));\
        }while(0)
#define LED_DISK_READ(x) \
         GIO_REG_BIT_SET(rw_pa_data_out, CONFIG_ETRAX_LED3G, !(x)) 
#endif

#ifdef CONFIG_ETRAX_PB_LEDS
#define LED_NETWORK_SET_G(x) \
         GIO_REG_BIT_SET(rw_pb_data_out, CONFIG_ETRAX_LED1G, !(x))
#define LED_NETWORK_SET_R(x) \
         GIO_REG_BIT_SET(rw_pb_data_out, CONFIG_ETRAX_LED1R, !(x))
#define LED_ACTIVE_SET_G(x) \
         GIO_REG_BIT_SET(rw_pb_data_out, CONFIG_ETRAX_LED2G, !(x))
#define LED_ACTIVE_SET_R(x) \
         GIO_REG_BIT_SET(rw_pb_data_out, CONFIG_ETRAX_LED2R, !(x))
#define LED_DISK_WRITE(x) \
        do{\
                GIO_REG_BIT_SET(rw_pb_data_out, CONFIG_ETRAX_LED3G, !(x));\
                GIO_REG_BIT_SET(rw_pb_data_out, CONFIG_ETRAX_LED3R, !(x));\
        }while(0)
#define LED_DISK_READ(x) \
         GIO_REG_BIT_SET(rw_pb_data_out, CONFIG_ETRAX_LED3G, !(x))     
#endif

#ifdef CONFIG_ETRAX_CSP0_LEDS
#define CONFIGURABLE_LEDS\
        ((1 << CONFIG_ETRAX_LED1G ) | (1 << CONFIG_ETRAX_LED1R ) |\
         (1 << CONFIG_ETRAX_LED2G ) | (1 << CONFIG_ETRAX_LED2R ) |\
         (1 << CONFIG_ETRAX_LED3G ) | (1 << CONFIG_ETRAX_LED3R ) |\
         (1 << CONFIG_ETRAX_LED4G ) | (1 << CONFIG_ETRAX_LED4R ) |\
         (1 << CONFIG_ETRAX_LED5G ) | (1 << CONFIG_ETRAX_LED5R ) |\
         (1 << CONFIG_ETRAX_LED6G ) | (1 << CONFIG_ETRAX_LED6R ) |\
         (1 << CONFIG_ETRAX_LED7G ) | (1 << CONFIG_ETRAX_LED7R ) |\
         (1 << CONFIG_ETRAX_LED8Y ) | (1 << CONFIG_ETRAX_LED9Y ) |\
         (1 << CONFIG_ETRAX_LED10Y ) |(1 << CONFIG_ETRAX_LED11Y )|\
         (1 << CONFIG_ETRAX_LED12R ))

#define LED_NETWORK_SET_G(x) \
         REG_SHADOW_SET(port_csp0_addr, port_csp0_shadow, CONFIG_ETRAX_LED1G, !(x))
#define LED_NETWORK_SET_R(x) \
         REG_SHADOW_SET(port_csp0_addr, port_csp0_shadow, CONFIG_ETRAX_LED1R, !(x))
#define LED_ACTIVE_SET_G(x) \
         REG_SHADOW_SET(port_csp0_addr, port_csp0_shadow, CONFIG_ETRAX_LED2G, !(x))
#define LED_ACTIVE_SET_R(x) \
         REG_SHADOW_SET(port_csp0_addr, port_csp0_shadow, CONFIG_ETRAX_LED2R, !(x))
#define LED_DISK_WRITE(x) \
        do{\
                REG_SHADOW_SET(port_csp0_addr, port_csp0_shadow, CONFIG_ETRAX_LED3G, !(x));\
                REG_SHADOW_SET(port_csp0_addr, port_csp0_shadow, CONFIG_ETRAX_LED3R, !(x));\
        }while(0)
#define LED_DISK_READ(x) \
         REG_SHADOW_SET(port_csp0_addr, port_csp0_shadow, CONFIG_ETRAX_LED3G, !(x))
#define LED_BIT_SET(x)\
        do{\
                if((( 1 << x) & CONFIGURABLE_LEDS)  != 0)\
                       REG_SHADOW_SET(port_csp0_addr, port_csp0_shadow, x, 1);\
        }while(0)
#define LED_BIT_CLR(x)\
        do{\
                if((( 1 << x) & CONFIGURABLE_LEDS)  != 0)\
                       REG_SHADOW_SET(port_csp0_addr, port_csp0_shadow, x, 0);\
        }while(0)
#endif

#
#ifdef CONFIG_ETRAX_SOFT_SHUTDOWN
#define SOFT_SHUTDOWN() \
          REG_SHADOW_SET(port_csp0_addr, port_csp0_shadow, CONFIG_ETRAX_SHUTDOWN_BIT, 1)
#else
#define SOFT_SHUTDOWN()
#endif

#endif
