#define MEMPCPY
#define memcpy __mempcpy
#include "memcpy.c"
weak_alias (__mempcpy, mempcpy)
