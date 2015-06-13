{-
data type declaration grammar goes here

currently using ADT.hs, SPJ's paper on boxed/unboxed values and inferrence
from STG code as a basis (i.e. a little hacky)

-}

import DavidParser
import AST
import ADT
import Data.Char

-- make it easy to group parsed and unparsed input together for later filtration
data Parsed a = Parsed (Def a) | Unparsed String deriving (Show)

--parse = manyGreedy (orEx dataDecl

dataDecl = litString "data" `xthen` boxedDecl `orEx` unboxedDecl `using` (either id id) `thenx` manyWhite `thenx` (literal ';')

boxedDecl :: Parser Char (Parsed a)
boxedDecl = boxedConID `ordered` tyVars `thenx` eqSym `ordered` boxedConstrs `using` f
  where f ((con, tyVars), dataCons) = Parsed (DataDef (TyCon True con tyVars dataCons))

-- match the type constructor name for a boxed constructor
boxedConID :: Parser Char Con
boxedConID = conID
-- consider a lookahead here? boxedDecl requires that eqSym follow boxedCon, which should
-- ensure that no '#' following the ID exists, but there is an incongruence between this
-- definition and what really *should* consitute a valid boxed type constructor name

-- upper case character followed by adjacent alphanumerics.
-- TODO: Worry about other valid symbols?
conID = uprChr `ordered` manyGreedy alphaNum `using` cons

unboxedCon :: Parser Char Con
unboxedCon = conID `ordered` litString "#" `using` (uncurry (++)) 

uprChr = satisfy isUpper
alphaNum = satisfy isAlphaNum

tyVars :: Parser Char [TyVar]
tyVars = manyGreedy (someWhite `xthen` tyVar)

tyVar = satisfy isLower `ordered` manyGreedy alphaNum `using` cons

boxedConstrs :: Parser Char [DataCon]
boxedConstrs = accept [DataCon True "DataConName" [MVar "TyVar Name"]] -- placeholder

unboxedDecl :: Parser Char (Parsed a)
unboxedDecl = unboxedCon `ordered` tyVars `thenx` eqSym `ordered` unboxedConstrs `using` f
  where f ((con, tyVars), dataCons) = Parsed (DataDef (TyCon False con tyVars dataCons))

unboxedConstrs :: Parser Char [DataCon]
unboxedConstrs = accept [DataCon False "DataConName#" [MVar "TyVar Name"]] -- placeholder


eqSym = literal '=' `between` manyWhite
whiteSp = anyEq " \n\t"
manyWhite = manyGreedy whiteSp
someWhite = someGreedy whiteSp
