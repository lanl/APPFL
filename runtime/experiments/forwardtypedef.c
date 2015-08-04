
struct _What;  // OPTIONAL

typedef struct _What What;

typedef What *What_p;

struct _Which {
  What *w;
  What_p p;
  // What q;  ERROR, size of type not known
};

struct _What {
  int i;
};
