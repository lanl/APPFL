#ifndef stgApply_h
#define stgApply_h
#include "stg.h"
FnPtr stgApplyN();
FnPtr stgApplyP();
FnPtr stgApplyNN();
FnPtr stgApplyPN();
FnPtr stgApplyNP();
FnPtr stgApplyPP();
FnPtr stgApplyNNN();
FnPtr stgApplyPNN();
FnPtr stgApplyNPN();
FnPtr stgApplyPPN();
FnPtr stgApplyNNP();
FnPtr stgApplyPNP();
FnPtr stgApplyNPP();
FnPtr stgApplyPPP();
FnPtr stgApplyNNNN();
FnPtr stgApplyPNNN();
FnPtr stgApplyNPNN();
FnPtr stgApplyPPNN();
FnPtr stgApplyNNPN();
FnPtr stgApplyPNPN();
FnPtr stgApplyNPPN();
FnPtr stgApplyPPPN();
FnPtr stgApplyNNNP();
FnPtr stgApplyPNNP();
FnPtr stgApplyNPNP();
FnPtr stgApplyPPNP();
FnPtr stgApplyNNPP();
FnPtr stgApplyPNPP();
FnPtr stgApplyNPPP();
FnPtr stgApplyPPPP();
FnPtr stgApplyNNNNN();
FnPtr stgApplyPNNNN();
FnPtr stgApplyNPNNN();
FnPtr stgApplyPPNNN();
FnPtr stgApplyNNPNN();
FnPtr stgApplyPNPNN();
FnPtr stgApplyNPPNN();
FnPtr stgApplyPPPNN();
FnPtr stgApplyNNNPN();
FnPtr stgApplyPNNPN();
FnPtr stgApplyNPNPN();
FnPtr stgApplyPPNPN();
FnPtr stgApplyNNPPN();
FnPtr stgApplyPNPPN();
FnPtr stgApplyNPPPN();
FnPtr stgApplyPPPPN();
FnPtr stgApplyNNNNP();
FnPtr stgApplyPNNNP();
FnPtr stgApplyNPNNP();
FnPtr stgApplyPPNNP();
FnPtr stgApplyNNPNP();
FnPtr stgApplyPNPNP();
FnPtr stgApplyNPPNP();
FnPtr stgApplyPPPNP();
FnPtr stgApplyNNNPP();
FnPtr stgApplyPNNPP();
FnPtr stgApplyNPNPP();
FnPtr stgApplyPPNPP();
FnPtr stgApplyNNPPP();
FnPtr stgApplyPNPPP();
FnPtr stgApplyNPPPP();
FnPtr stgApplyPPPPP();
FnPtr stgApplyNNNNNN();
FnPtr stgApplyPNNNNN();
FnPtr stgApplyNPNNNN();
FnPtr stgApplyPPNNNN();
FnPtr stgApplyNNPNNN();
FnPtr stgApplyPNPNNN();
FnPtr stgApplyNPPNNN();
FnPtr stgApplyPPPNNN();
FnPtr stgApplyNNNPNN();
FnPtr stgApplyPNNPNN();
FnPtr stgApplyNPNPNN();
FnPtr stgApplyPPNPNN();
FnPtr stgApplyNNPPNN();
FnPtr stgApplyPNPPNN();
FnPtr stgApplyNPPPNN();
FnPtr stgApplyPPPPNN();
FnPtr stgApplyNNNNPN();
FnPtr stgApplyPNNNPN();
FnPtr stgApplyNPNNPN();
FnPtr stgApplyPPNNPN();
FnPtr stgApplyNNPNPN();
FnPtr stgApplyPNPNPN();
FnPtr stgApplyNPPNPN();
FnPtr stgApplyPPPNPN();
FnPtr stgApplyNNNPPN();
FnPtr stgApplyPNNPPN();
FnPtr stgApplyNPNPPN();
FnPtr stgApplyPPNPPN();
FnPtr stgApplyNNPPPN();
FnPtr stgApplyPNPPPN();
FnPtr stgApplyNPPPPN();
FnPtr stgApplyPPPPPN();
FnPtr stgApplyNNNNNP();
FnPtr stgApplyPNNNNP();
FnPtr stgApplyNPNNNP();
FnPtr stgApplyPPNNNP();
FnPtr stgApplyNNPNNP();
FnPtr stgApplyPNPNNP();
FnPtr stgApplyNPPNNP();
FnPtr stgApplyPPPNNP();
FnPtr stgApplyNNNPNP();
FnPtr stgApplyPNNPNP();
FnPtr stgApplyNPNPNP();
FnPtr stgApplyPPNPNP();
FnPtr stgApplyNNPPNP();
FnPtr stgApplyPNPPNP();
FnPtr stgApplyNPPPNP();
FnPtr stgApplyPPPPNP();
FnPtr stgApplyNNNNPP();
FnPtr stgApplyPNNNPP();
FnPtr stgApplyNPNNPP();
FnPtr stgApplyPPNNPP();
FnPtr stgApplyNNPNPP();
FnPtr stgApplyPNPNPP();
FnPtr stgApplyNPPNPP();
FnPtr stgApplyPPPNPP();
FnPtr stgApplyNNNPPP();
FnPtr stgApplyPNNPPP();
FnPtr stgApplyNPNPPP();
FnPtr stgApplyPPNPPP();
FnPtr stgApplyNNPPPP();
FnPtr stgApplyPNPPPP();
FnPtr stgApplyNPPPPP();
FnPtr stgApplyPPPPPP();
#define STGAPPLYN(f,v1) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 1}; \
  STGJUMP3(stgApplyN,N,f,v1); \
  } while(0)

#define STGAPPLYP(f,v1) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 1}; \
  STGJUMP3(stgApplyP,N,f,v1); \
  } while(0)

#define STGAPPLYNN(f,v1,v2) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 2}; \
  STGJUMP4(stgApplyNN,N,f,v1,v2); \
  } while(0)

#define STGAPPLYPN(f,v1,v2) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 2}; \
  STGJUMP4(stgApplyPN,N,f,v1,v2); \
  } while(0)

#define STGAPPLYNP(f,v1,v2) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 2}; \
  STGJUMP4(stgApplyNP,N,f,v1,v2); \
  } while(0)

#define STGAPPLYPP(f,v1,v2) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 2}; \
  STGJUMP4(stgApplyPP,N,f,v1,v2); \
  } while(0)

#define STGAPPLYNNN(f,v1,v2,v3) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 3}; \
  STGJUMP5(stgApplyNNN,N,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYPNN(f,v1,v2,v3) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 3}; \
  STGJUMP5(stgApplyPNN,N,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYNPN(f,v1,v2,v3) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 3}; \
  STGJUMP5(stgApplyNPN,N,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYPPN(f,v1,v2,v3) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 3}; \
  STGJUMP5(stgApplyPPN,N,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYNNP(f,v1,v2,v3) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 3}; \
  STGJUMP5(stgApplyNNP,N,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYPNP(f,v1,v2,v3) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 3}; \
  STGJUMP5(stgApplyPNP,N,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYNPP(f,v1,v2,v3) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 3}; \
  STGJUMP5(stgApplyNPP,N,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYPPP(f,v1,v2,v3) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 3}; \
  STGJUMP5(stgApplyPPP,N,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYNNNN(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyNNNN,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPNNN(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyPNNN,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNPNN(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyNPNN,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPPNN(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyPPNN,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNNPN(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyNNPN,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPNPN(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyPNPN,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNPPN(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyNPPN,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPPPN(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyPPPN,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNNNP(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyNNNP,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPNNP(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyPNNP,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNPNP(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyNPNP,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPPNP(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyPPNP,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNNPP(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyNNPP,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPNPP(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyPNPP,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNPPP(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyNPPP,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPPPP(f,v1,v2,v3,v4) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 4}; \
  STGJUMP6(stgApplyPPPP,N,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNNNNN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNNNNN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNNNN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPNNNN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPNNN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNPNNN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPNNN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPPNNN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNPNN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNNPNN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNPNN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPNPNN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPPNN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNPPNN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPPNN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPPPNN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNNPN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNNNPN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNNPN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPNNPN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPNPN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNPNPN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPNPN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPPNPN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNPPN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNNPPN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNPPN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPNPPN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPPPN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNPPPN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPPPN(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPPPPN,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNNNP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNNNNP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNNNP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPNNNP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPNNP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNPNNP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPNNP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPPNNP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNPNP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNNPNP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNPNP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPNPNP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPPNP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNPPNP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPPNP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPPPNP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNNPP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNNNPP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNNPP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPNNPP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPNPP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNPNPP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPNPP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPPNPP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNPPP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNNPPP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNPPP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPNPPP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPPPP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyNPPPP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPPPP(f,v1,v2,v3,v4,v5) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 5}; \
  STGJUMP7(stgApplyPPPPP,N,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNNNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNNNNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNNNNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPNNNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPNNNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNPNNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNPNNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPPNNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPPNNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNNPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNNPNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNNPNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPNPNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPNPNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNPPNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNPPNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPPPNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPPPNN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNNNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNNNPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNNNPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPNNPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPNNPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNPNPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNPNPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPPNPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPPNPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNNPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNNPPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNNPPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPNPPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPNPPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNPPPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNPPPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPPPPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPPPPN,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNNNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNNNNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNNNNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPNNNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPNNNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNPNNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNPNNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPPNNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPPNNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNNPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNNPNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNNPNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPNPNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPNPNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNPPNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNPPNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPPPNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPPPNP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNNNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNNNPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNNNPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPNNPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPNNPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNPNPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNPNPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPPNPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPPNPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNNPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNNPPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNNPPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPNPPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPNPPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNNPPPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPNPPPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyNPPPPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  PtrOrLiteral N = {.argType = INT, .i = 6}; \
  STGJUMP8(stgApplyPPPPPP,N,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#endif
