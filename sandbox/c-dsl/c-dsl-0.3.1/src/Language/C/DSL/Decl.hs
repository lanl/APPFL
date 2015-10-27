{-# LANGUAGE FlexibleInstances #-}
module Language.C.DSL.Decl where
import Language.C
import Data.String
import Language.C.DSL.StringLike

-- | A low level way to declare something.
decl :: CDeclSpec       -- ^ The declaration specifier, usually this is a type
        -> CDeclr      -- ^ Equivalent to the name of the object being declared. Often this will
                       -- make use of the overloaded string instance for 'CDeclr's
        -> Maybe CExpr -- ^ The optional init expression
        -> CDecl
decl ty name exp = CDecl [ty] [(Just name, flip CInitExpr undefNode `fmap` exp, Nothing)] undefNode

-- | The 'CTypeSpec' for @void@
voidSpec  :: CTypeSpec
voidSpec = CVoidType undefNode

-- | The 'CTypeSpec' for @char@
charSpec  :: CTypeSpec
charSpec = CCharType undefNode

-- | The 'CTypeSpec' for @short@
shortSpec :: CTypeSpec
shortSpec  = CShortType undefNode

-- | The 'CTypeSpec' for @int@
intSpec :: CTypeSpec
intSpec = CIntType undefNode

-- | The 'CTypeSpec' for @long@
longSpec :: CTypeSpec
longSpec = CLongType undefNode

-- | The 'CTypeSpec' for @float@
floatSpec :: CTypeSpec
floatSpec  = CFloatType undefNode

-- | The 'CTypeSpec' for @double@
doubleSpec :: CTypeSpec
doubleSpec = CDoubleType undefNode


-- | The 'CDeclSpec' for declarations of type @void@
voidTy  :: CDeclSpec
voidTy = CTypeSpec $ CVoidType undefNode

-- | The 'CDeclSpec' for declarations of type @char@
charTy  :: CDeclSpec
charTy = CTypeSpec $ CCharType undefNode

-- | The 'CDeclSpec' for declarations of type @short@
shortTy :: CDeclSpec
shortTy  = CTypeSpec $ CShortType undefNode

-- | The 'CDeclSpec' for declarations of type @int@
intTy :: CDeclSpec
intTy = CTypeSpec $ CIntType undefNode

-- | The 'CDeclSpec' for declarations of type @long@
longTy :: CDeclSpec
longTy = CTypeSpec $ CLongType undefNode

-- | The 'CDeclSpec' for declarations of type @float@
floatTy :: CDeclSpec
floatTy  = CTypeSpec $ CFloatType undefNode

-- | The 'CDeclSpec' for declarations of type @double@
doubleTy :: CDeclSpec
doubleTy = CTypeSpec $ CDoubleType undefNode


-- | Turns a string into the corresponding typedefed type.
-- 
-- For example
-- 
-- > struct "foo" [("bar, ty "quux")]
-- 
-- will generate the corresponding
-- 
-- > typedef foo {quux bar;} foo
ty :: Ident -> CTypeSpec
ty = flip CTypeDef undefNode

-- | Modifies a declarator to be a pointer. For example
-- @ptr someName@ would be @*x@ in C.
ptr :: CDeclr -> CDeclr
ptr (CDeclr nm mods cstr attrs node) = CDeclr nm (CPtrDeclr [] undefNode : mods) cstr attrs node

-- | A short cut for declaring a @char@.
-- 
-- >     char "x" .= 1
-- >     uninit $ char "y"
--
-- Would generate
--
-- > char x = 1;
-- > char y;
char :: CDeclr -> Maybe CExpr -> CDecl
char   = decl charTy

-- | A short cut for declaring a @short@
short :: CDeclr -> Maybe CExpr -> CDecl
short  = decl shortTy

-- | A short cut for declaring a @int@
int :: CDeclr -> Maybe CExpr -> CDecl
int    = decl intTy

-- | A short cut for declaring a @long@
long :: CDeclr -> Maybe CExpr -> CDecl
long   = decl longTy

-- | A short cut for declaring a @float@
float :: CDeclr -> Maybe CExpr -> CDecl
float  = decl floatTy

-- | A short cut for declaring a @double@
double :: CDeclr -> Maybe CExpr -> CDecl
double = decl doubleTy

-- | Equivalent to @'char'@ but wraps the @'CDeclr'@ in a pointer.
-- This means that @uninit $ charPtr someName@ is equivalent to @char *someName;@
charPtr  :: CDeclr -> Maybe CExpr -> CDecl
charPtr   = char . ptr

shortPtr :: CDeclr -> Maybe CExpr -> CDecl
shortPtr  = short . ptr

intPtr   :: CDeclr -> Maybe CExpr -> CDecl
intPtr    = int . ptr

longPtr  :: CDeclr -> Maybe CExpr -> CDecl
longPtr   = long . ptr

floatPtr :: CDeclr -> Maybe CExpr -> CDecl
floatPtr  = float . ptr

doublePtr:: CDeclr -> Maybe CExpr -> CDecl
doublePtr = double . ptr


-- | Supplies an initializer for an for a declaration. This
-- is meant to be used with the 'char' and friends short cuts
(.=) :: (Maybe CExpr -> CDecl) -> CExpr -> CDecl
f .= e = f (Just e)
infixl 7 .=

-- | Leave a declaration uninitialized. This is meant to be used
-- with the 'char' and friends declaration
uninit :: (Maybe CExpr -> CDecl) -> CDecl
uninit = ($ Nothing)

csu :: CStructTag -> String -> [(String, CTypeSpec)] -> CDecl
csu tag ident fields = CDecl
                       [CStorageSpec $ CTypedef undefNode, CTypeSpec $ CSUType structTy undefNode]
                       [(Just $ fromString ident, Nothing, Nothing)]
                       undefNode
  where structTy  = CStruct tag (Just $ fromString ident) (Just $ map structify fields) [] undefNode
        structify (name, ty) = CDecl [CTypeSpec ty] [(Just (fromString name), Nothing, Nothing)] undefNode

-- | Create a structure, for example @struct "foo" [("bar", intTy)]@ is
-- @typedef struct foo {int bar;} foo;@
struct ::  String -> [(String, CTypeSpec)] -> CDecl
struct = csu CStructTag

-- | Equivalent to 'struct' but generates a C union instead.
union :: String -> [(String, CTypeSpec)] -> CDecl
union  = csu CUnionTag

-- | Defines a C function. For example
-- 
-- >    test =
-- >       fun [intTy] "test"[int "a", int "b"] $ hblock [
-- >           creturn ("a" + "b")
-- >       ]
-- 
-- Would be the equivalent of
-- 
-- >   int test(int a, int b)
-- >   {
-- >      return a + b;
-- >   }
fun :: [CDeclSpec] -> String -> [Maybe CExpr -> CDecl] -> CStat -> CFunDef
fun specs name args body = annotatedFun specs name args [] body

-- | Identical to fun except this annotates the list of attributes given
-- as a list of strings.
annotatedFun :: [CDeclSpec] -> String -> [Maybe CExpr -> CDecl] -> [String] -> CStat -> CFunDef
annotatedFun specs name args annots body = CFunDef specs decl [] body undefNode
  where decl = CDeclr (Just $ fromString name)
               [CFunDeclr (Right (fmap ($Nothing) args, False)) [] undefNode]
               Nothing attrs undefNode
        attrs :: [CAttr]
        attrs = map (\ s -> CAttr (fromString s) [] undefNode) annots

class External a where
  export :: a -> CExtDecl
instance External CFunDef where
  export = CFDefExt
instance External CDecl where
  export = CDeclExt
instance External CStrLit where
  export = flip CAsmExt undefNode

-- | Exports a series of declarations to a translation unit.
transUnit :: [CExtDecl] -> CTranslUnit
transUnit = flip CTranslUnit undefNode
