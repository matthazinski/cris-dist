/*
 * Stub version of access.
 */

#include "config.h"
#include <_ansi.h>
#include <_syslist.h>
#include <sys/fcntl.h>
#include <errno.h>
#undef errno
extern int errno;
#include "warning.h"

#define w(x) write (2, x, strlen (x))
int _access (char *p)
{
  w ("_access called; unimplemented: ");
  w (p);
  w ("\n");
  errno = EFAULT;
  return -1;
}

int access (char *p)
{
  w ("access called; unimplemented: ");
  w (p);
  w ("\n");
  errno = EFAULT;
  return -1;
}

stub_warning(_access)
stub_warning(access)
