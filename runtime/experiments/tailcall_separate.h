#ifndef tailcall_separate_h
#define tailcall_separate_h

#define JUMP0(f)          f(NULL, 0, 0, 0, 0)
#define JUMP1(f,x)        f(NULL, x, 0, 0, 0)
#define JUMP2(f,x,y)      f(NULL, x, y, 0, 0)
#define JUMP3(f,x,y,z)    f(NULL, x, y, z, 0)
#define JUMP4(f,x,y,z,t)  f(NULL, x, y, z, t)

#define STGRETURN0()	     \
  switch ((stgSP--)->argc) {				\
  case 0 : return JUMP0(((stgSP+1)->fp));			\
  case 1 : return JUMP1(((stgSP+1)->fp), (stgSP+1)->pl[0]);		\
  case 2 : return JUMP2(((stgSP+1)->fp), (stgSP+1)->pl[0], (stgSP+1)->pl[1]);		\
  case 3 : return JUMP3(((stgSP+1)->fp), (stgSP+1)->pl[0], (stgSP+1)->pl[1], (stgSP+1)->pl[2]);		\
  case 4 : return JUMP4(((stgSP+1)->fp), (stgSP+1)->pl[0], (stgSP+1)->pl[1], (stgSP+1)->pl[2], (stgSP+1)->pl[3]); \
  }

typedef struct {
  int whatever;
  double whatever2;
} Obj;

typedef void (*Fun4)(Obj * /* self */, uint64_t, uint64_t, uint64_t, uint64_t /* args */);

typedef struct {
  Fun4 fp;
  int argc;
  uint64_t pl[5];
} Cont;

extern Cont stgStack[10];
extern Cont *stgSP;

#endif
