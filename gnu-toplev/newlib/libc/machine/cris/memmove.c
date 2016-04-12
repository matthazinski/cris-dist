/* A memmove for CRIS.
   Copyright 2000, 2002 Axis Communications.  */

/* FIXME: This file should really only be used for reference, as the
   result is somewhat depending on gcc generating what we expect rather
   than what we describe.  An assembly file should be used instead.

   Even worse, we base it on memcpy, on the assumption that overlapping
   moves are rare, and we will do no worse than the generic memmove.  */

#include <stddef.h>

/* Break even between movem and move16 is really at 38.7 * 2, but
   modulo 44, so up to the next multiple of 44, we use ordinary code.  */
#define MEMMOVE_BY_BLOCK_THRESHOLD (44 * 2)

/* No name ambiguities in this file.  */
__asm__ (".syntax no_register_prefix");

void *
memmove(void *pdst,
       const void *psrc,
       size_t pn)
{
  /* Now we want the parameters put in special registers.
     Make sure the compiler is able to make something useful of this.
     As it is now: r10 -> r13; r11 -> r11 (nop); r12 -> r12 (nop).

     If gcc was allright, it really would need no temporaries, and no
     stack space to save stuff on.
     FIXME: Why void *?  Why not char *?  */

  register void *return_dst __asm__ ("r10") = pdst;
  register void *dst __asm__ ("r13") = pdst;
  register const void *src __asm__ ("r11") = psrc;
  register int n __asm__ ("r12") = pn;

  /* Check and handle overlap.  */
  if ((char *) src < (char *) dst && (char *) dst < (char *) src + n)
    {
      /* Destructive overlap.  We could optimize this, but we don't (for
	 the moment).  */
      src += n;
      dst += n;
      while (n--)
	{
	  *--((char *) dst) = *--((char *) src);
	}

      return return_dst;
    }
  /* Whew, no overlap.  Proceed as with mempcy.  We could call it instead
     of having a copy here.  That would spoil some of the optimization, so
     we take the trouble with having two copies.  */

  /* First make a simple byte copy to get the alignmnt right.  */
  if (((unsigned long) src & (unsigned long) dst & 1)
      /* Oops! n = 0 must be a valid call, regardless of alignment.  */
     && n != 0)
    {
      /* FIXME: Why not dword alignment?  */
      *(char *) dst = *(char *) src;
      src++;
      dst++;
      n--;
    }

  /* Decide which copying method to use.  */
  if (n >= MEMMOVE_BY_BLOCK_THRESHOLD)
    {
      /* It is not optimal to tell the compiler about clobbering any
	 registers; that will move the saving/restoring of those registers
	 to the function prologue/epilogue, and make non-movem sizes
	 suboptimal.

	 This method is not foolproof; it assumes that the "asm reg"
	 declarations at the beginning of the function really are used
	 here (beware: they may be moved to temporary registers).
	 This way, we do not have to save/move the registers around into
	 temporaries; we can safely use them straight away.

	 If you want to check that the allocation was right; then
	 check the equalities in the first comment.  It should say
	 "r13=r13, r11=r11, r12=r12".  */
      __asm__ volatile
	("\
	 ;; Check that the following is true (same register names on	\n\
	 ;; both sides of equal sign, as in r8=r8):			\n\
	 ;; %0=r13, %1=r11, %2=r12					\n\
	 ;;								\n\
	 ;; Save the registers we'll use in the movem process		\n\
	 ;; on the stack.						\n\
	 subq	11*4,sp							\n\
	 movem	r10,[sp]						\n\
									\n\
	 ;; Now we've got this:						\n\
	 ;; r11 - src							\n\
	 ;; r13 - dst							\n\
	 ;; r12 - n							\n\
									\n\
	 ;; Update n for the first loop.				\n\
	 subq	 44,r12							\n\
0:									\n\
"
#ifdef __arch_common_v10_v32
	 /* Cater to branch offset difference between v32 and v10.  We
	    assume the branch below has an 8-bit offset.  */
"	 setf\n"
#endif	 
"	 movem	[r11+],r10						\n\
	 subq	44,r12							\n\
	 bge	 0b							\n\
	 movem	r10,[r13+]						\n\
									\n\
	 ;; Compensate for last loop underflowing n.			\n\
	 addq	44,r12							\n\
									\n\
	 ;; Restore registers from stack.				\n\
	 movem [sp+],r10"

	 /* Outputs.  */
	 : "=r" (dst), "=r" (src), "=r" (n)

	 /* Inputs.  */
	 : "0" (dst), "1" (src), "2" (n));
    }

  while (n >= 16)
    {
      *((long *) dst)++ = *((long *) src)++;
      *((long *) dst)++ = *((long *) src)++;
      *((long *) dst)++ = *((long *) src)++;
      *((long *) dst)++ = *((long *) src)++;

      n -= 16;
    }

  switch (n)
    {
    case 0:
      break;

    case 1:
      *(char *) dst = *(char *) src;
      break;

    case 2:
      *(short *) dst = *(short *) src;
      break;

    case 3:
      *((short *) dst)++ = *((short *) src)++;
      *(char *) dst = *(char *) src;
      break;

    case 4:
      *((long *) dst)++ = *((long *) src)++;
      break;

    case 5:
      *((long *) dst)++ = *((long *) src)++;
      *(char *) dst = *(char *) src;
      break;

    case 6:
      *((long *) dst)++ = *((long *) src)++;
      *(short *) dst = *(short *) src;
      break;

    case 7:
      *((long *) dst)++ = *((long *) src)++;
      *((short *) dst)++ = *((short *) src)++;
      *(char *) dst = *(char *) src;
      break;

    case 8:
      *((long *) dst)++ = *((long *) src)++;
      *((long *) dst)++ = *((long *) src)++;
      break;

    case 9:
      *((long *) dst)++ = *((long *) src)++;
      *((long *) dst)++ = *((long *) src)++;
      *(char *) dst = *(char *) src;
      break;

    case 10:
      *((long *) dst)++ = *((long *) src)++;
      *((long *) dst)++ = *((long *) src)++;
      *(short *) dst = *(short *) src;
      break;

    case 11:
      *((long *) dst)++ = *((long *) src)++;
      *((long *) dst)++ = *((long *) src)++;
      *((short *) dst)++ = *((short *) src)++;
      *(char *) dst = *(char *) src;
      break;

    case 12:
      *((long *) dst)++ = *((long *) src)++;
      *((long *) dst)++ = *((long *) src)++;
      *((long *) dst)++ = *((long *) src)++;
      break;

    case 13:
      *((long *) dst)++ = *((long *) src)++;
      *((long *) dst)++ = *((long *) src)++;
      *((long *) dst)++ = *((long *) src)++;
      *(char *) dst = *(char *) src;
      break;

    case 14:
      *((long *) dst)++ = *((long *) src)++;
      *((long *) dst)++ = *((long *) src)++;
      *((long *) dst)++ = *((long *) src)++;
      *(short *) dst = *(short *) src;
      break;

    case 15:
      *((long *) dst)++ = *((long *) src)++;
      *((long *) dst)++ = *((long *) src)++;
      *((long *) dst)++ = *((long *) src)++;
      *((short *) dst)++ = *((short *) src)++;
      *(char *) dst = *(char *) src;
      break;
    }

  return return_dst;
}

