{-# LANGUAGE FlexibleInstances #-}
-- | This module contians the DSL for writing @CExpr@s.
-- It doesn't export the orphan instance for @IsString CExpr@ which
-- can be found in "Language.C.DSL.StringLike".
module Language.C.DSL.Exp where
import Language.C
import Data.String
import Language.C.DSL.StringLike

-- | Lift a Haskell string into a literal C string.
str :: String -> CExpr
str = CConst . flip CStrConst undefNode . cString

cOp :: CBinaryOp -> CExpr -> CExpr -> CExpr
cOp op a b = CBinary op a b undefNode

-- | Equality test, @a ==: b@ is equivalent to @a == b@
(==:) :: CExpr -> CExpr -> CExpr
(==:) = cOp CEqOp

-- | Inequality test, @a /=: b@ is equivalent to @a != b@
(/=:) :: CExpr -> CExpr -> CExpr
(/=:) = cOp CNeqOp

-- | Less-than test, @a <: b@ is equivalent to @a < b@
(<:) :: CExpr -> CExpr -> CExpr
(<:)  = cOp CLeOp

-- | Greater-than test, @a >: b@ is equivalent to @a > b@
(>:) :: CExpr -> CExpr -> CExpr
(>:)  = cOp CGrOp

-- | Less than or equal to, @a <=: b@ is equivalent to @a <= b@
(<=:) :: CExpr -> CExpr -> CExpr
(<=:) = cOp CLeqOp

-- | Greater than or equal to, @a >=: b@ is equivalent to @a >= b@
(>=:) :: CExpr -> CExpr -> CExpr
(>=:) = cOp CGeqOp

-- | The ternary operator in C. @ternary a b c@ will turn into @a ? b : c@.
ternary :: CExpr -> CExpr -> CExpr -> CExpr
ternary i t e = CCond i (Just t) e undefNode

-- | An orphan @Num@ instance for @CExpr@. It provides inline implementations
-- of @abs@ and @signum@ that don't rely on @math.h@.
instance Num CExpr where
  fromInteger = CConst . flip CIntConst undefNode . cInteger
  (*)         = cOp CMulOp
  (+)         = cOp CAddOp
  (-)         = cOp CSubOp
  abs a       = ternary (a >=: 0) a (negate a)
  signum a    = ternary (a >=: 0) (ternary (a ==: 0) a 1) (-1)
instance Fractional CExpr where
  (/)          = cOp CDivOp
  fromRational = CConst . flip CFloatConst undefNode . cFloat . fromRational

-- | A function mapping identifier in C to be used as variables. Normally this can be
-- avoided since "Language.C.DSL.StringLike" provides an 'IsString' instance.
var :: Ident -> CExpr
var = flip CVar undefNode

-- | Function calls, @f#[a, b, c]@ will become @f(a, b, c)@. Note
-- that @f@ is also an expression.
(#) :: CExpr -> [CExpr] -> CExpr
f # args = CCall f args undefNode

-- | The assignment operator. @var <-- value@ will become @var = value;@ in C.
(<--) :: CExpr -> CExpr -> CExpr
var <-- val = CAssign CAssignOp var val undefNode
infixl 3 <--

-- | This is the more generalized version of '(<--)'. It allows
-- any 'CAssignOp' to be passed in to facilitate writing @a += b@ and
-- similar.
assign :: CAssignOp -> CExpr -> CExpr -> CExpr
assign mode var val = CAssign mode var val undefNode


-- | A simplified unary operator type. It
-- can be converted to 'Language.C's version using
-- 'toCUnaryOp'.
data UnOp = PlusPlus
          | MinusMinus
          | Minus
          | Plus
          | Not
          | Addr -- ^ The address of operator @&@.
          | Ind -- ^ The dereferencing operator in C @*@.
          deriving(Eq, Show)

-- | Convert a 'UnOp' to the corresponding 'CUnaryOp'.
toCUnaryOp :: UnOp -> CUnaryOp
toCUnaryOp Minus = CMinOp
toCUnaryOp Plus  = CPlusOp
toCUnaryOp Not   = CNegOp
toCUnaryOp Addr  = CAdrOp
toCUnaryOp Ind   = CIndOp

-- | Apply a unary operator prefix, @op `pre` exp@ will transform into something like @op exp@
-- in C. This only matters for 'PlusPlus' and 'MinusMinus'.
pre   :: UnOp -> CExpr -> CExpr
PlusPlus   `pre` exp = CUnary CPreIncOp exp undefNode
MinusMinus `pre` exp = CUnary CPreDecOp exp undefNode
op         `pre` exp = CUnary (toCUnaryOp op) exp undefNode

-- | The postfix equivalent of 'pre'.
post  :: CExpr -> UnOp -> CExpr
exp `post` PlusPlus   = CUnary CPostIncOp exp undefNode
exp `post` MinusMinus = CUnary CPostDecOp exp undefNode
exp `post` op         = CUnary (toCUnaryOp op) exp undefNode

-- | A quick wrapper of @pre Ind exp@ since it's so common.
star :: CExpr -> CExpr
star = pre Ind

-- | The C comma operator, @comma [a, b, c]@ is equivalent to @a, b, c@ in C.
comma :: [CExpr] -> CExpr
comma = flip CComma undefNode

-- | Implements C style casts for expressions.
castTo :: CExpr -> CDecl -> CExpr
exp `castTo` ty = CCast ty exp undefNode

-- | @size of@ for types. 
sizeOfDecl :: CDecl -> CExpr
sizeOfDecl = flip CSizeofType undefNode

-- | @size of@ for expressions. Carefully note that @sizeOf "someType"@ will
-- incorrectly treat @someType@ as a variable, not a type.
sizeOf :: CExpr -> CExpr
sizeOf = flip CSizeofExpr undefNode

-- | Access a field of a struct, this C's @.@ operator.
(&) :: CExpr -> String -> CExpr
struct & field = CMember struct (fromString field) False undefNode
infixl 8 &

-- | The automatic dereferencing @->@ in C.
(&*) :: CExpr -> String -> CExpr
struct &* field = CMember struct (fromString field) True undefNode
infixl 8 &*

-- | This is the indexing operator in C, @a ! i@ is @a[i]@.
(!) :: CExpr -> CExpr -> CExpr
arr ! ind = CIndex arr ind undefNode
infixl 8 !
