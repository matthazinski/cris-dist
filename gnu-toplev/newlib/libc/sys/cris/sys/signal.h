/* sys/signal.h */

#ifndef _SYS_SIGNAL_H
#define _SYS_SIGNAL_H

#ifdef __cplusplus
extern "C" {
#endif

#include "_ansi.h"

#ifndef __STRICT_ANSI__
typedef unsigned long sigset_t;

/* Adjusted to linux, has unused sa_restorer field and unsigned long
   sa_flags; relatively unimportant though.  */
/* Type of a signal handler.  */
typedef void (*__sighandler_t)(int);

struct sigaction {
	__sighandler_t sa_handler;
	sigset_t sa_mask;
	unsigned long sa_flags;
	void (*sa_restorer)(void);
};

/* Adjusted to glibc; other values.  */
#define SA_NOCLDSTOP 1	/* only value supported now for sa_flags */
#define SIG_SETMASK 2	/* set mask with sigprocmask() */
#define SIG_BLOCK 0	/* set of signals to block */
#define SIG_UNBLOCK 1	/* set of signals to, well, unblock */

/* These depend upon the type of sigset_t, which right now 
   is always a long.. They're in the POSIX namespace, but
   are not ANSI. */
#define sigaddset(what,sig) (*(what) |= (1<<(sig)))
#define sigemptyset(what)   (*(what) = 0)

int sigprocmask (int __how, const sigset_t *__a, sigset_t *__b);

/* protos for functions found in winsup sources */
#if defined(__CYGWIN32__)
#undef sigaddset
#undef sigemptyset
/* The first argument to kill should be pid_t.  Right now
   <sys/types.h> always defines pid_t to be int.  If that ever
   changes, then we will need to do something else, perhaps along the
   lines of <machine/types.h>.  */
int _EXFUN(kill, (int, int));
int _EXFUN(sigaction, (int, const struct sigaction *, struct sigaction *));
int _EXFUN(sigaddset, (sigset_t *, const int));
int _EXFUN(sigdelset, (sigset_t *, const int));
int _EXFUN(sigismember, (const sigset_t *, int));
int _EXFUN(sigfillset, (sigset_t *));
int _EXFUN(sigemptyset, (sigset_t *));
int _EXFUN(sigpending, (sigset_t *));
int _EXFUN(sigsuspend, (const sigset_t *));
int _EXFUN(sigpause, (int));
#endif

#endif /* __STRICT_ANSI__ */

#define SIGHUP		 1
#define SIGINT		 2
#define SIGQUIT		 3
#define SIGILL		 4
#define SIGTRAP		 5
#define SIGABRT		 6
#define SIGIOT		 6
#define SIGBUS		 7
#define SIGFPE		 8
#define SIGKILL		 9
#define SIGUSR1		10
#define SIGSEGV		11
#define SIGUSR2		12
#define SIGPIPE		13
#define SIGALRM		14
#define SIGTERM		15
#define SIGSTKFLT	16
#define SIGCHLD		17
#define SIGCONT		18
#define SIGSTOP		19
#define SIGTSTP		20
#define SIGTTIN		21
#define SIGTTOU		22
#define SIGURG		23
#define SIGXCPU		24
#define SIGXFSZ		25
#define SIGVTALRM	26
#define SIGPROF		27
#define SIGWINCH	28
#define SIGIO		29
#define SIGPOLL		SIGIO
#define SIGPWR		30
#define	NSIG 31

#ifdef __cplusplus
}
#endif
#endif /* _SYS_SIGNAL_H */
