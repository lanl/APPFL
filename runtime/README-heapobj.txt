This is intended to document the structure of heap objects.

For now we will assume a 64-bit architecture, *nix.  Whether or not
we will be tied to gcc, gcc or LLVM, etc., remains to be seen.

We have fixed in stone that there will be a universal fixed-size data
type that can exist outside the heap (TODO: need to characterize this better)
called PtrOrLiteral (TODO: or maybe PtrOrLit later).  Initially, for
simplicity and debugging purposes, it will contain a discrimator tag.
In principle this is not necessary because of strong static typing.  Thus
we will not manipulate, for example, unboxed array types (though we could
have boxed arrays of unboxed Ints).

typedef enum {
  INT, 
  DOUBLE, 
  HEAPOBJ 
} ArgType;

// PtrOrLiteral -- pointer to heap object or literal value
typedef struct {
  ArgType argType;        // superfluous, for sanity checking
  union {
    int i;
    double d;
    Obj *op;
  };
} PtrOrLiteral;

There are two kinds of heap objects:  those dynamically allocated on the heap,
which will be referred to simply as heap objects or HOs, and those statically
allocated--embedded as global variables in the generated program--which
are referred to as static heap objects or SHOs.  Both are of type Obj.

There are six types of heap objects (type Obj): FUN, PAP, CON, THUNK,
BLACKHOLE, and INDIRECT.

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

It must be possible to determine the size of a heap object dynamically,
given only a pointer to it (this does not apply to (SHOs)), so that it can
be copied during garbage collection, because at some point heap objects
will become variably sized, not just between object types (FUN, PAP, etc.),
but for each type as well.

For all HO types except PAP the object size can be determined from solely from
the "info pointer", a pointer to a static struct (of type InfoTab) containing
information about the object.  So that an unknown object can be identified,
its first field will be the info pointer.

A PAP's info pointer is its underlying FUN info pointer.  The size of PAP
changes dynamically as it applied to successively more arguments .  Thus it
will have an embedded field indicating how many arguments it has already been
applied to, and thus via this information and the FUN info pointer its size
may be determined.

All HOs have three parts (using field names, not the field
name C types):

-----------------------------------------------------
| infoPtr | objType |             payload           |
-----------------------------------------------------

While the ObjType is also in the InfoTab, we choose to have a dedicated
field in the HO as well for reasons that will be detailed later.

Now for the layouts of each object type.  In the following, "free variables"
means a sequence PtrOrLiteral where field argType = HEAPOBJ, and "arguments"
means PtrOrLiteral where argType can be any of the ArgType values.

FUN

-----------------------------------------------------
| infoPtr | objType |        free variables         |
-----------------------------------------------------

The number of free variables is in infoPtr->funFields.fvCount (TODO: currently
it's infoPtr->fvCount).  objType = FUN and infoPtr->objType = FUN.

PAP

-------------------------------------------------------------------------------------
| infoPtr | objType | free variables | #args already applied | args already applied |
-------------------------------------------------------------------------------------

TODO:  currently #args already applied is a distinct field in every object.

The PAP infoPtr points is its underlying FUN infoPtr.  Thus objType = PAP but
infoPtr->objType = FUN.  The the number of free variables is still
infoPtr->funFields.fvCount (TODO: currently it's infoPtr->fvCount).

CON

-----------------------------------------------------
| infoPtr | objType |        arguments              |
-----------------------------------------------------

The number of arguments is infoPtr->conFields.argCount, objType = CON and
infoPtr->objType = CON.

THUNK

-----------------------------------------------------
| infoPtr | objType |        free variables         |
-----------------------------------------------------

The number of free variables is in infoPtr->thunkFields.fvCount (TODO: currently
it's infoPtr->fvCount).  objType = THUNK and infoPtr->objType = THUNK.  

BLACKHOLE

Black holes only come from THUNKs.  For convenience and arguable efficiency,
a BLACKHOLE will be created by simply overwriting objType with value BLACKHOLE,
so infoPtr->objType = THUNK.  

INDIRECT

INDIRECTs arise from BLACKHOLEs being updated.  Again, we will simply overwrite
objType with value INDIRECT, and infoPtr->objType = THUNK.  


Efficiency and Simplicity Considerations

We use an extra field in the object to store ObjType.  For BLACKHOLE this
is not necessary because we could have an infoTab entry for BLACKHOLE.  The
implies, however, creating a new object (and discarding the old) with the
invocation of every THUNK.  The saving of space, given the small fixed size
of the hypothetically distinct BLACKHOLE object, does not seem worthwhile
given the extra generation of garbage and the frequency of THUNK invocation.

The size of a PAP cannot be statically determined, but we do need to know
the number of arguments to which it has been applied.  We could conflate
the FUN and PAP type if "#args already applied" were stored in the FUN object,
with value 0 meaning it's still a FUN, but the distinction would then be
immaterial.
