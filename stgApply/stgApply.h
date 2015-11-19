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
  STGJUMP2(stgApplyN,f,v1); \
  } while(0)

#define STGAPPLYP(f,v1) \
do { \
  STGJUMP2(stgApplyP,f,v1); \
  } while(0)

#define STGAPPLYNN(f,v1,v2) \
do { \
  STGJUMP3(stgApplyNN,f,v1,v2); \
  } while(0)

#define STGAPPLYPN(f,v1,v2) \
do { \
  STGJUMP3(stgApplyPN,f,v1,v2); \
  } while(0)

#define STGAPPLYNP(f,v1,v2) \
do { \
  STGJUMP3(stgApplyNP,f,v1,v2); \
  } while(0)

#define STGAPPLYPP(f,v1,v2) \
do { \
  STGJUMP3(stgApplyPP,f,v1,v2); \
  } while(0)

#define STGAPPLYNNN(f,v1,v2,v3) \
do { \
  STGJUMP4(stgApplyNNN,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYPNN(f,v1,v2,v3) \
do { \
  STGJUMP4(stgApplyPNN,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYNPN(f,v1,v2,v3) \
do { \
  STGJUMP4(stgApplyNPN,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYPPN(f,v1,v2,v3) \
do { \
  STGJUMP4(stgApplyPPN,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYNNP(f,v1,v2,v3) \
do { \
  STGJUMP4(stgApplyNNP,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYPNP(f,v1,v2,v3) \
do { \
  STGJUMP4(stgApplyPNP,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYNPP(f,v1,v2,v3) \
do { \
  STGJUMP4(stgApplyNPP,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYPPP(f,v1,v2,v3) \
do { \
  STGJUMP4(stgApplyPPP,f,v1,v2,v3); \
  } while(0)

#define STGAPPLYNNNN(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyNNNN,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPNNN(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyPNNN,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNPNN(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyNPNN,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPPNN(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyPPNN,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNNPN(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyNNPN,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPNPN(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyPNPN,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNPPN(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyNPPN,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPPPN(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyPPPN,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNNNP(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyNNNP,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPNNP(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyPNNP,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNPNP(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyNPNP,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPPNP(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyPPNP,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNNPP(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyNNPP,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPNPP(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyPNPP,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNPPP(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyNPPP,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYPPPP(f,v1,v2,v3,v4) \
do { \
  STGJUMP5(stgApplyPPPP,f,v1,v2,v3,v4); \
  } while(0)

#define STGAPPLYNNNNN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNNNNN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNNNN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPNNNN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPNNN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNPNNN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPNNN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPPNNN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNPNN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNNPNN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNPNN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPNPNN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPPNN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNPPNN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPPNN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPPPNN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNNPN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNNNPN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNNPN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPNNPN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPNPN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNPNPN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPNPN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPPNPN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNPPN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNNPPN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNPPN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPNPPN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPPPN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNPPPN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPPPN(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPPPPN,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNNNP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNNNNP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNNNP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPNNNP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPNNP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNPNNP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPNNP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPPNNP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNPNP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNNPNP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNPNP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPNPNP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPPNP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNPPNP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPPNP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPPPNP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNNPP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNNNPP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNNPP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPNNPP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPNPP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNPNPP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPNPP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPPNPP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNPPP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNNPPP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPNPPP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPNPPP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNPPPP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyNPPPP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYPPPPP(f,v1,v2,v3,v4,v5) \
do { \
  STGJUMP6(stgApplyPPPPP,f,v1,v2,v3,v4,v5); \
  } while(0)

#define STGAPPLYNNNNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNNNNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNNNNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPNNNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPNNNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNPNNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNPNNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPPNNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPNNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPPNNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNNPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNNPNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNNPNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPNPNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPNPNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNPPNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNPPNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPPPNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPPNN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPPPNN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNNNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNNNPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNNNPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPNNPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPNNPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNPNPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNPNPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPPNPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPNPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPPNPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNNPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNNPPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNNPPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPNPPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPNPPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNPPPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNPPPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPPPPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPPPN(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPPPPN,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNNNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNNNNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNNNNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPNNNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPNNNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNPNNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNPNNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPPNNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPNNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPPNNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNNPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNNPNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNNPNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPNPNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPNPNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNPPNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNPPNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPPPNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPPNP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPPPNP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNNNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNNNPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNNNPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPNNPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPNNPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNPNPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNPNPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPPNPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPNPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPPNPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNNPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNNPPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNNPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNNPPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPNPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPNPPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPNPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPNPPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNNPPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNNPPPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPNPPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPNPPPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYNPPPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyNPPPPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#define STGAPPLYPPPPPP(f,v1,v2,v3,v4,v5,v6) \
do { \
  STGJUMP7(stgApplyPPPPPP,f,v1,v2,v3,v4,v5,v6); \
  } while(0)

#endif
