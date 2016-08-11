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
static inline int64_t charAtIndex (char *str, int64_t idx) {
  return str[idx];
}

/* This might also be done with a cast to unsigned, but I'm not sure if that's
   platform/implementation dependent. */
static inline int64_t intLogicalRightShift (int64_t trg, int64_t shift) {

  asm("movb %[shift], %%cl\n\t"
      "shrq %%cl, %[trg]"
      : [trg] "+g" (trg)
      : [shift] "g" (shift)
      : "cl"
    );

  return trg;
  /* alternative, if Clang doesn't like inline assembly */
  /* int64_t mask = ((1 << 31) >> shift) << 1; */
  /* return (trg >> shift) & (~mask); */

}

#endif
