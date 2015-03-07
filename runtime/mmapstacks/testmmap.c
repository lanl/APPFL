#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h> // for  mmap()
#include <fcntl.h>
#include <unistd.h> // for 
#include <stdio.h>
#include <errno.h>
#include <string.h>  // for memcpy()
#include <stdlib.h>  // for exit()
#include <malloc.h>  // for memalign()

#define MAX_SIZE_T           (~(size_t)0)
#define MFAIL                ((void*)(MAX_SIZE_T))

const size_t cmmStackSize = (size_t)100*(size_t)1024*(size_t)1024*(size_t)1024;

// char *bigbufm;

int main(int argc, char **argv) {
  size_t pagesize = (size_t) sysconf(_SC_PAGE_SIZE);
  printf("page size is %zd\n", pagesize );
  printf("sizeof(off_t)=%lu\n", sizeof(off_t));


  void *
    fp1 = mmap( NULL,                   // void *address, NULL => no preference
	       cmmStackSize,           // size_t length
	       PROT_READ | PROT_WRITE, // int protect, write may require read
	       MAP_PRIVATE | MAP_ANONYMOUS | MAP_GROWSDOWN | MAP_STACK, // int flags
	       -1,                     // int filedes
	       0 );                    // off_t offset

  void *
    fp2 = mmap( NULL,                   // void *address, NULL => no preference
	       cmmStackSize,           // size_t length
	       PROT_READ | PROT_WRITE, // int protect, write may require read
	       MAP_PRIVATE | MAP_ANONYMOUS | MAP_GROWSDOWN | MAP_STACK, // int flags
	       -1,                     // int filedes
	       0 );                    // off_t offset

  void *
    fp3 = mmap( NULL,                   // void *address, NULL => no preference
	       cmmStackSize,           // size_t length
	       PROT_READ | PROT_WRITE, // int protect, write may require read
	       MAP_PRIVATE | MAP_ANONYMOUS | MAP_GROWSDOWN | MAP_STACK, // int flags
	       -1,                     // int filedes
	       0 );                    // off_t offset

  /*
  void *
    fp = mmap( (void *)bigbufm,        // void *address, NULL => no preference
	       usablesize,             // size_t length
	       PROT_READ | PROT_WRITE, // int protect, write may require read
	       MAP_SHARED | MAP_FIXED, // int flags, MAP_SHARED => changes written back to file
	       fd,                     // int filedes
	       0 );                    // off_t offset
  */
    printf("mmap produced with %p %p %p\n", fp1, fp2, fp3);
  /*
  if (fp == MAP_FAILED) {
    printf("mmap failed!!\n"); exit(1);
  } else {
    printf("mmap succeeded with %p %p %p\n", fp1, fp2, fp3);
  }
  */
  /*
  char s[256];
  size_t i, j, k;
  for (k=0; k!=2; k++)
  for(i = 0; i != 16; i++) {
    printf("next...\n");
    for (j=0; j != 1024*1024*1024; j++)
      bigbuf[i*1024*1024*1024+j] = 'a';
    // bigbufm[i*1024*1024*1024+j] = 'a';
  }
  */

  printf("done!\n");
}
  
  
