This is intended to document the structure of heap/continuation stack objects.

For now we will assume a 64-bit architecture, *nix.  Whether or not we will be
tied to gcc, gcc or LLVM, etc., remains to be seen.  There is no fundamental
reason for not supporting 32-bit other than effort.

We have fixed in stone that there will be a universal fixed-size data type
that can exist outside the heap, called PtrOrLiteral (TODO: or maybe PtrOrLit
later).  Unboxed types may be multiples of this.  Initially, for simplicity
and debugging purposes, it will contain a discrimator tag.  This tag will be
eliminated now that we have typing information.  Currently the tag discrimates
C int, C double (float if 32-bit supported), and a pointer to a heap object
(type Obj).  Absent the discriminator tag this will be a 64-bit (32-bit)
entity.

typedef enum {
  INT, 
  DOUBLE,
  // FLOAT,
  HEAPOBJ 
} ArgType;

// PtrOrLiteral -- literal value or pointer to heap object
typedef struct {
  ArgType argType;  -- will be eliminated
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

For all HO types except PAP the positions of the valid PtrOrLiteral values
that may be pointers to HOs can be determined from solely from the "info
pointer", a pointer to a static struct (of type InfoTab) containing
information about the object.  So that an unknown object can be identified,
its first field will be the info pointer.  For now it's a named field,
"infoPtr", in the Obj type.

A PAP's info pointer is its underlying FUN info pointer.  The number of valid
PtrOrLiterals in a PAP grows dynamically as it applied to successively more
arguments.  Thus we include a field in the object itself indicating how many
of these arguments it currently has.

struct _Obj;

typedef struct _Obj Obj;

struct _Obj {
  InfoTab *infoPtr;         // canonical location of ObjType field
  ObjType objType;          // to distinguish PAP, FUN, BLACKHOLE, INDIRECT
  PtrOrLiteral payload[];   // variably sized
};


All HOs have three parts (using Obj field names, not the field name C types):

C type
-------------------------------------------------------------
| infoPtr | objType |          |          payload           |
-------------------------------------------------------------

While the ObjType is also in the InfoTab, we choose to have a dedicated field
in the HO as well for reasons that will be detailed later.

Next the layouts of each object type.  In the following, "free variables"
means a sequence PtrOrLiteral where field argType = HEAPOBJ, and "arguments"
means PtrOrLiteral where argType can be any of the ArgType values.

FUN

For FUN the payload is the list of its lexically free variables,
i.e. PtrOrLiterals to HOs, starting at payload[0].

          |                    | payload 
--------------------------------------------------------------------------------
| infoPtr | objType |          | boxed free variables | unboxed free variables |
--------------------------------------------------------------------------------

objType = FUN

infoPtr->objType = FUN
infoPtr->fvCount is number of free variables
infoPtr->layoutInfo.boxedCount is number of boxed free variables
infoPtr->layoutInfo.unboxedCount is number of unboxed free variables

PAP
                    | payload
                    | [0..fvCount-1] |  [fvCount]  |
----------------------------------------------------------------------
| infoPtr | objType | bfvs  |  ubfvs | layout_info | bargs | ubargs  |
----------------------------------------------------------------------

The PAP infoPtr points is its underlying FUN infoTab entry, so

objType = PAP 

infoPtr->objType = FUN
infoPtr->fvCount = number of free variables 
infoPtr->layoutInfo.boxedCount is number of boxed free variables
infoPtr->layoutInfo.unboxedCount is number of unboxed free variables
layout_info:  see PNPACK and PNUNPACK in codegen/stg.h

CON
                               | payload 
-------------------------------------------------------------------------
| infoPtr | objType |          | boxed args | unboxed args              |
-------------------------------------------------------------------------

infoPtr->conFields.arity = number of args 
infoPtr->objType = CON
infoPtr->layoutInfo.boxedCount is number of boxed ARGUMENTS
infoPtr->layoutInfo.unboxedCount is number of unboxed ARGUMENTS

infoPtr->payloadSize = infoPtr->layoutInfo.boxedCount + infoPtr->layoutInfo.unboxedCount


THUNK
                    | payload[0] | payload[1..fvCount+1] 
----------------------------------------------------------------------------------
| infoPtr | objType |   result   |  boxed free variables | unboxed free variables|
----------------------------------------------------------------------------------

Note that a THUNK must have a payload size of at least 1 so that it
can become and INDIRECT.

objType = THUNK

infoPtr->objType = THUNK
infoPtr->fvCount = number of free variables
infoPtr->layoutInfo.boxedCount is number of boxed free variables
infoPtr->layoutInfo.unboxedCount is number of unboxed free variables

BLACKHOLE
                               | payload 
-----------------------------------------------------------------------------------
| infoPtr | objType |          | result | bfvs (they're still live!) | ubfvs      |
-----------------------------------------------------------------------------------

Black holes only come from THUNKs.  For convenience and arguable efficiency, a
BLACKHOLE will be created by simply overwriting objType with value BLACKHOLE,

objType = BLACKHOLE

infoPtr->objType = THUNK
infoPtr->fvCount = number of free variables
infoPtr->layoutInfo.boxedCount is number of boxed free variables
infoPtr->layoutInfo.unboxedCount is number of unboxed free variables

INDIRECT
                               |       payload[0]         |
------------------------------------------------------------------------
| infoPtr | objType |          | ptr to some other object | irrelevant |
------------------------------------------------------------------------

INDIRECTs arise from BLACKHOLEs being updated.  The THUNK object is again
reused, so 

objType = INDIRECT

infoPtr->objType = THUNK
infoPtr->fvCount = ***NO LONGER VALID, ANY FREE VARS WILL BE CAPTURED ELSEWHERE***
payload[0] = the pointer to the next object in the indirect chain
payload[1..] = garbage

FORWARD
 
Forwarding pointers appear temporarily during garbage collection.
The infoPtr of the Obj is replaced by a ptr to the new Obj in the "To space"
with the least significant bit set to 1 to distinguish a forwarding pointer 
from an info pointer.


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
  //
  ...
} ObjType;


Continuation stack objects are manipulated in the following places.

UPDCONT  - runtime/ stg.h, stg.c, gc.c
           codegen/ CodeGen.hs

CASECONT - codegen/Codegen.hs, cge _ ECase
           codegen/CodeGen.hs, cgalts _ Alts

CALLCONT - runtime/ stg.h, stg.c, stgutils.h, stgutils.c, gc.c
           codegen/ CodeGen.hs

FUNCONT  - <none yet>

The layout of continuation stack objects is as follows.

TODO:  objType field will go away, should use a pair of macros
  SETOBJTYPE and GETOBJTYPE

UPDCONT
                               | payload[0]
-----------------------------------------------------------------------------
| infoPtr | objType |          | ptr to object to update                    |
-----------------------------------------------------------------------------

objType = UPDCONT
payload[0] is ptr to object to update, i.e. is a root


CASECONT
                               | payload[0..n] 
---------------------------------------------------
| infoPtr | objType |          | fv_0 | .. | fv_n | pointers first
---------------------------------------------------

objType = CASECONT
payload[0]..payload[infoPtr->fvCount] are the free vars
infoPtr->layoutInfo.boxedCount = #pointers
infoPtr->layoutInfo.unboxedCount = #non-pointers

infoPtr->layoutInfo.boxedCount + infoPtr->layoutInfo.unboxedCount = infoPtr->fvCount, fvCount GOING AWAY

CALLCONT
                               | payload[0] | payload[1..payload[0]] 
-----------------------------------------------------------------------------
| infoPtr | objType |          | #pointers  | just pointers                 |
-----------------------------------------------------------------------------

NOTE:  infoPtr->fvCount is INVALID, #free vars is embedded in payload
objType = CALLCONT
payload[0] = # free vars
payload[1]..payload[payload[0]] are the free vars


FUNCONT - IGNORE FOR NOW, not yet implemented
                               | payload 
-----------------------------------------------------------------------------
| infoPtr | objType |          | single free var                            |
-----------------------------------------------------------------------------

objType = FUNCONT infoPtr->objType = FUN payload[0] is the sole free var (the
function to be applied), i.e. is the only root.  NOT CORRECT?  should be
all other args not currently being evaluated?


------------------------------------------------------------------------------
InfoTab
-------

The interpretation of LayoutInfo varies by object type.
TODO:  should it be in FUNfields/PAPfields etc.?

FUN, PAP, THUNK:  boxedCount is number of boxed free variables

typedef struct {
  int payloadSize;
  int boxedCount;
  int unboxedCount;
} LayoutInfo;

typedef struct {
  int arity;
  // curry paper suggests that we need type info
} FUNfields;

typedef struct {
} PAPfields;

typedef struct {
  int tag;
  int arity;
  char conName[64];
} CONfields;

typedef struct {
} THUNKfields;

typedef struct {
} UPDCONTfields;

typedef struct {
} CASECONTfields;

typedef struct {
} CALLCONTfields;

typedef struct {
} FUNCONTfields;

struct _InfoTab {
  char name[32];  // for debugging
  int fvCount;    // lexically determined, should be in layout
  CmmFnPtr entryCode; 
  ObjType objType; // kind of object, tag for union
  LayoutInfo layoutInfo;
  union {
    FUNfields funFields;
    PAPfields papFields;
    CONfields conFields;
    THUNKfields thunkFields;
    UPDCONTfields updcontFields;
    CASECONTfields casecontFields;
    CALLCONTfields callcontFields;
    FUNCONTfields funcontFields;
  };
};



------------------------------------------------------------------------------

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

typedef enum {
  // heap objects
  ...
  // stack objects
  ...
  // garbage collection
  FORWARD
} ObjType;

We have an ObjType value for the sole use of the GC.  If objtype = FORWARD,
the object is assumed to have been already copied to the "to" space (or
somewhere, depending on GC scheme), so its HO pointers and infoPtr have been
copied, so it it is safe to use payload[0] as the forwarding pointer.

A prototypical sequence of events would be

1) Copy object from "from" space to "to" space
2) from_space_object->objType assigned FORWARD
3) from_space_object->payload[0] assigned to_space_object

Steps 2 and 3 could occur in either order.

