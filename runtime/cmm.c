#include "assert.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h> // for  mmap()
#include <fcntl.h>
#include <unistd.h> // for 
#include <stdio.h>
#include <errno.h>
#include <string.h>  // for memcpy()
#include <stdlib.h>  // for exit()
#ifndef __APPLE__
#include <malloc.h>  // for memalign()
#endif

#include "cmm.h"

