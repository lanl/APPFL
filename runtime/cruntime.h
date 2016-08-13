#ifndef cruntime_h
#define cruntime_h

static inline void raise() {
   fprintf(stderr,"raise stub\n");
   exit(1);
}


static inline int imin( int x, int y ) {
  return x <= y ? x : y;
}

static inline int imax( int x, int y ) {
  return x >= y ? x : y;
}

#endif
