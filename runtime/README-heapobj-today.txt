This is intended to document the structure of heap objects.

For now we will assume a 64-bit architecture, *nix.  Whether or not we will be
tied to gcc, gcc or LLVM, etc., remains to be seen.

We have fixed in stone that there will be a universal fixed-size data type
that can exist outside the heap (TODO: need to characterize this better)
called PtrOrLiteral (TODO: or maybe PtrOrLit later).  Initially, for
simplicity and debugging purposes, it will contain a discrimator tag.  In
principle this is not necessary because of strong static typing.  Thus we will
not manipulate, for example, unboxed array types (though we could later have
boxed arrays of unboxed Ints).  Currently the tag discrimates C int, C double,
and a pointer to a heap object (type Obj).

typedef enum {
  INT, 
  DOUBLE, 
  HEAPOBJ 
} ArgType;

// PtrOrLiteral -- literal value or pointer to heap object
typedef struct {
  ArgType argType;
  union {
    int i;
    double d;
    Obj *op;   //object pointer
  };
} PtrOrLiteral;

There are two kinds of heap objects:  those dynamically allocated on the heap,
which will be referred to simply as heap objects or HOs, and those statically
allocated--embedded as global variables in the generated program--which
are referred to as static heap objects or SHOs.  Both are of type Obj.

There are six types of (static) heap objects: FUN, PAP, CON, THUNK, BLACKHOLE,
and INDIRECT.

typedef enum {
  // heap objects
  FUN, 
  PAP, 
  CON,
  THUNK,
  BLACKHOLE,
  INDIRECT,
  // stack objects
  ...
} ObjType;

In general it must be possible to determine the size of a heap object
dynamically, given only a pointer to it (this does not apply to (SHOs)), so
that it can be copied during garbage collection, because at some point heap
objects will become variably sized, not just between object types (FUN, PAP,
etc.), but for each type as well.  

For now all HOs are of uniform fixed size.

Even with uniform size, for garbage collection purposes we need to be able to
look inside objects and deterministically determine the pointers to other HOs
they may contain.  (Note: an HO may contain a pointer to itself.)

For all HO types except PAP the positions of the valid PtrOrLiteral values
that may be pointers to HOs can be determined from solely from the "info
pointer", a pointer to a static struct (of type InfoTab) containing
information about the object.  So that an unknown object can be identified,
its first field will be the info pointer.  For now it's a named field,
"infoPtr", in the Obj type.

A PAP's info pointer is its underlying FUN info pointer.  The number of valid
PtrOrLiterals in a PAP grows dynamically as it applied to successively more
arguments.  Thus we include a field in the object itself indicating how many
of this arguments it currently has.

struct _Obj;

typedef struct _Obj Obj;

struct _Obj {
  InfoTab *infoPtr;         // canonical location of ObjType field
  ObjType objType;          // to distinguish PAP, FUN, BLACKHOLE, INDIRECT
  int argCount;             // for PAP, how many args already applied to?
  PtrOrLiteral payload[16]; // fixed for now
};


All HOs have three parts (using Obj field names, not the field name C types):

Obj type
-------------------------------------------------------------
| infoPtr | objType | argCount |          payload           |
-------------------------------------------------------------

While the ObjType is also in the InfoTab, we choose to have a dedicated field
in the HO as well for reasons that will be detailed later.

NOTE!!!!:  argCount is for PAP only.  Later we will embed it in payload.

Next the layouts of each object type.  In the following, "free variables"
means a sequence PtrOrLiteral where field argType = HEAPOBJ, and "arguments"
means PtrOrLiteral where argType can be any of the ArgType values.

FUN

For FUN the payload is the list of its lexically free variables,
i.e. PtrOrLiterals to HOs, starting at payload[0].

                               | payload 
-----------------------------------------------------------------------------
| infoPtr | objType | argCount | free variables                             |
-----------------------------------------------------------------------------

The number of free variables is in infoPtr->fvCount.  objType = FUN and
infoPtr->objType = FUN.

PAP

-----------------------------------------------------------------------------
| infoPtr | objType | argCount | free variables | args already applied      |
-----------------------------------------------------------------------------

The PAP infoPtr points is its underlying FUN infoTab entry.  Thus objType =
PAP but infoPtr->objType = FUN.  The the number of free variables is still
infoPtr->fvCount.

CON

                               | payload 
-----------------------------------------------------------------------------
| infoPtr | objType | argCount | arguments                                  |
-----------------------------------------------------------------------------

The number of arguments is infoPtr->conFields.argCount, objType = CON and
infoPtr->objType = CON.

THUNK

                               | payload 
-----------------------------------------------------------------------------
| infoPtr | objType | argCount | free variables                             |
-----------------------------------------------------------------------------

The number of free variables is infoPtr->fvCount.  objType = THUNK and
infoPtr->objType = THUNK.

BLACKHOLE

Black holes only come from THUNKs.  For convenience and arguable efficiency,
a BLACKHOLE will be created by simply overwriting objType with value BLACKHOLE,
so infoPtr->objType = THUNK.

INDIRECT

                               | payload 
-----------------------------------------------------------------------------
| infoPtr | objType | argCount | ptr to some other object                   |
-----------------------------------------------------------------------------

INDIRECTs arise from BLACKHOLEs being updated.  The THUNK object is reused, so
infoPtr->objType = THUNK and we will simply set objType = INDIRECT.

For now, payload[0] is the pointer to the next object in the indirect chain.



Roots for Garbage Collection

We'll write GC for both "garbage collection" and "garbage collector."

Roots for GC are guaranteed to all be in the SHO and the continuation stack.
The roots are all the PtrOrLiterals where .objType = HEAPOBJ.  Elements of the
continuation stack (continuation stack objects, CSOs) are also of type Obj,
containing PtrOrLiterals, which if pointers are roots.

The only variations on Cheney's algorithm are as follows:

- Since SHOs and HOs can point to SHOs, and SHOs don't get copied, this must
be taken into consideration.  Merely regard SHOs as "already copied."  The "to
do list" (those objects between "Ptr" and "Free" in "to space") will still be
correct even in the absense of copies of SHOs precisely because the SHOs are
roots.

- GC time is convenient to eliminate indirection chains.  All this requires
is when dereferencing a PtrOrLiteral is always do something like this

Obj *derefHO(Obj *op) {
  while (op->objType == INDIRECT)
    op = op->payload[0].op;
  return op;
}

Obj* derefPoL(PtrOrLiteral f) {
  assert(f.argType == HEAPOBJ && "derefPoL: not a HEAPOBJ");
  return derefHO(f.op);
}


Static Heap Objects

Pointers to all the SHOs will be gathered up in an array like so:

void initPredefs() {
  stgStatObj[stgStatObjCount++] = &sho_unit;
  stgStatObj[stgStatObjCount++] = &sho_one;
  stgStatObj[stgStatObjCount++] = &sho_two;
  stgStatObj[stgStatObjCount++] = &sho_main_thunk_unit;
  stgStatObj[stgStatObjCount++] = &sho_main1;
  stgStatObj[stgStatObjCount++] = &sho_main3;
  stgStatObj[stgStatObjCount++] = &sho_main5;
  stgStatObj[stgStatObjCount++] = &sho_main4;
  stgStatObj[stgStatObjCount++] = &sho_id;
  stgStatObj[stgStatObjCount++] = &sho_constf;
  stgStatObj[stgStatObjCount++] = &sho_mainfail;
}



Continuation Stack Objects

typedef enum {
  // heap objects
  ...
  // stack objects
  UPDCONT, 
  CASECONT, 
  CALLCONT, 
  FUNCONT        // for strict evaluation
} ObjType;

The layout of continuation stack objects is as follows.


UPDCONT
                               | payload 
-----------------------------------------------------------------------------
| infoPtr | objType | argCount | ptr to object to update                    |
-----------------------------------------------------------------------------

objType = UPDCONT
payload[0] is ptr to object to update, i.e. is a root


CASECONT
                               | payload 
-----------------------------------------------------------------------------
| infoPtr | objType | argCount | free vars                                  |
-----------------------------------------------------------------------------

objType = CASECONT
infoPtr->fvCount is number of free vars in payload, as usual


CALLCONT
                               | payload 
-----------------------------------------------------------------------------
| infoPtr | objType | argCount | #free vars | free vars                     |
-----------------------------------------------------------------------------

NOTE:  infoPtr->fvCount is INVALID, #free vars is embedded in payload
objType = CALLCONT
payload[0] = # free vars
payload[1]..payload[payload[0]] are the free vars


FUNCONT
                               | payload 
-----------------------------------------------------------------------------
| infoPtr | objType | argCount | single free var                            |
-----------------------------------------------------------------------------

objType = FUNCONT infoPtr->objType = FUN payload[0] is the sole free var (the
function to be applied), i.e. is the only root.


