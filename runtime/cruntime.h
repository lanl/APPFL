#ifndef cruntime_h
#define cruntime_h

static inline int imin( int x, int y ) {
  return x <= y ? x : y;
}

static inline int imax( int x, int y ) {
  return x >= y ? x : y;
}

/* Implementation of 
   PIdxChar in our land,
   indexCharOffAddr# in GHC land */
static inline int charAtIndex (char *str, int idx) {
  return str[idx];
}

/* This might also be done with a cast to unsigned, but I'm not sure if that's
   platform/implementation dependent. */
static inline int intLogicalRightShift (int trg, int shift) {
  // Much thanks to the CS:APP course by CMU
  int mask = ((1 << 31) >> shift) << 1;
  return (trg >> shift) & (~mask);
}

#endif
