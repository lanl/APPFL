{-
data type declaration grammar goes here

currently using ADT.hs, SPJ's paper on boxed/unboxed values and inferrence
from STG code and haskell's rules as a basis (i.e. a little hacky)


<dataDefs> ::= <dataDef> (";" <dataDef>)*

<dataDef>  ::= "data" ["unboxed"] <binding>

<binding>  ::= <typeCon> "=" <dataCons>

<typeCon>  ::= <conName> (<typeVar>)*

<dataCons> ::= <dataCon> ("|" <dataCon>)*

<dataCon>  ::= <conName> (<monoType>)*

<monoType> ::= <typeVar>
             | <funType>
             | <conType>

<funType>  ::= "(" <monoType> ("->" <monoType>)+ ")"

<conType>  ::= <conName>
             | <qualCon>

<qualCon>  ::= "(" <conName> (<monoType>)* ")" -- qualified Constructor (e.g. Maybe a)

<conName>  ::= <upperChar> (<idChar>)*

<typeVar>  ::= <lowerChar> (<idChar>)*

<idChar>   ::= <alphaNum>
             | "#"
          -- | [others?]

-}

{- todo: implement cuts

fail hard at several levels or only at "atomic" level?
 - if all subparts pass, whole should pass
 - more meaningful messages at lower levels

fail in
beforeVars if conNameP doesn't match
datacon    if conNameP doesn't match
arrowPart  if arrowP matches but following monoTyp does not

-}

module DataParser
(
  dataDefP,
  toConMaps,
) where

import DavidParser
import Tokenizer
import qualified Data.Map as Map
import ADT

        
dataDefP = dataP `xthen` tyConP `thenx` eqP `ordered` dataConsP `thenx` scP `using` f
  where f = DataDef . (uncurry ($))

tyConP = beforeVars `ordered` (many' varNameP) `using` (uncurry ($))
  where
    beforeVars = boxityP `ordered` conNameP `using` (uncurry TyCon)
    boxityP = optionally (rsvP "unboxed" `using` (const False)) True
    
dataConsP = dataConP `ordered` (many' $ barP `xthen` dataConP) `using` cons

dataConP = conNameP `ordered` many' monoTypP `using` (uncurry DataCon)

monoTypP = orExList [varNameP `using` MVar,
                     mFunP `using` (uncurry MFun),
                     mConP `using` (uncurry MCon),
                     inparensP monoTypP]

mFunP = let arrowPart = some' (arrowP `xthen` monoTypP) `using` (foldr1 MFun)
        in inparensP $ monoTypP `ordered` arrowPart

mConP = orExList [conNameP `using` (flip (,) $ []),
                  inparensP $ conNameP `ordered` many' monoTypP]


toParamPairs t@(TyCon boxed tnm vars dCons) =
  let tPair =
        (tnm,
         TyConParam { tarity = length vars,
                      ttag   = error "no tag set",
                      tboxed = boxed,
                      tdatacons = map dName dCons,
                      tycon = t})
      dHelp tyName boxed d@(DataCon dnm mtypes) =
        (dnm,
         DataConParam { darity = length mtypes,
                        dtag = error "no tag set",
                        dboxed = boxed,
                        dtycon = tyName,
                        datacon = d})
      dPairs = map (dHelp tnm boxed) dCons
      dName (DataCon n _) = n
  in
   (tPair, dPairs)


toConMaps tycons =
  let
    pairs = map toParamPairs tycons
    tconMap = Map.fromList $ map fst pairs
    dconMap = Map.fromList $ concatMap snd pairs
  in
   (tconMap, dconMap)
    
        
                                
                                
                              
