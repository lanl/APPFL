{-# LANGUAGE NamedFieldPuns #-}

{-
data type declaration grammar goes here

currently using ADT.hs, SPJ's paper on boxed/unboxed values and inferrence
from STG code as a basis (i.e. a little hacky)

-}

import DavidParser
import Tokenizer
import ADT

-- make it easy to group parsed and unparsed input together for later filtration
data Parsed a = Parsed (Def ()) | Unparsed a deriving (Show)

testParse fileName =
  do
    file <- readFile fileName
    let ((parsed,_):_) = parse $ tokenize file
    mapM_ (putStrLn.show) parsed
    return ()

failTok p m inp = case p inp of
  [] -> error $
        "\nError" ++ (if null inp then ":" else " at " ++ show (head inp) ++":") ++ m
  x  -> x

parse = defs `thenx` (failTok eofP "EOF not found")
defs = manyGreedy $ (dataDecl `orEx` notEOF) `using` (either Parsed Unparsed)
  where notEOF inp = case inp of
          (TokEOF{}:_) -> reject inp
          (i:is)       -> accept i is
          []           -> reject inp
        


dataDecl = dataP `xthen` tyConP `thenx` eqP `ordered` dataConP `thenx` scP `using` f
  where f = DataDef . (uncurry ($))

{- todo: implement cuts

fail hard at several levels or only at "atomic" level?
 - if all subparts pass, whole should pass
 - more meaningful messages at lower levels

fail in
beforeVars if conNameP doesn't match
datacon    if conNameP doesn't match
arrowPart  if arrowP matches but following monoTyp does not

-}

tyConP = beforeVars `ordered` tyVars `using` (uncurry ($))
  where
    tyVars = optionally (manyGreedy varP) [] 
    beforeVars = boxityP `ordered` conNameP `using` (uncurry TyCon)
    boxityP = optionally (rsvP "unboxed" `using` (const False)) True
    

dataConP = datacon `ordered` optionally (barP `xthen` dataConP) [] `using` cons
  where
    datacon = conNameP `ordered` manyGreedy monoTyp `using` (uncurry DataCon)
    monoTyp = mVar `orEx` nestedMT `using` (either id id)
    nestedMT = inparens mFun `orEx` mCon `using` (either id id)
    mFun = monoTyp `ordered` arrowPart `using` (uncurry MFun)
    arrowPart = someGreedy (arrowP `xthen` monoTyp) `using` (foldr1 MFun)
    mCon = simpleCon `orEx` inparens complexCon `using` (either ($[]) id)
    simpleCon = conNameP `using` MCon
    complexCon = conNameP `ordered` (manyGreedy monoTyp) `using` (uncurry MCon)
    mVar = varP `orEx` inparens monoTyp `using` (either MVar id)

