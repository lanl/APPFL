#ifndef SANITY_H
#define SANITY_H

#include <ctype.h>

void heapCheck(bool display);
void stackCheck(bool display);

static inline char *getIdent(Obj *obj) {
  #if USE_IDENT
    return obj->ident;
  #else
    return NULL;
  #endif
}

static inline bool checkObjType(Obj *obj, ObjType objType) {
  #if USE_OBJTYPE
    return (obj->objType == objType);
  #else
    return true;
  #endif
}

static inline bool checkInfoTabName(InfoTab *it, char *name) {
  #if USE_INFOTAB_NAME
    return (strcmp(it->name, "stgIndirect") == 0);
  #else
    return true;
  #endif
}

static inline bool checkIdent(Obj *obj) {
  #if USE_IDENT
    for (int i = 0; obj->ident[i] != '\0'; i++) {
      assert(i < IDENT_SIZE && "sanity: bad ident size");
      if(!isprint(obj->ident[i])) return false;
    }
  #endif
  return true;
}

static inline bool checkInfoTabHeader(InfoTab *it) {
  #if DEBUG_INFOTAB
    return (it->pi = PI());
  #else
    return true;
  #endif
}

#endif
