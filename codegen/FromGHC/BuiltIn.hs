{-# LANGUAGE PatternGuards #-}

module FromGHC.BuiltIn
  ( module FromGHC.BuiltIn
  , voidPrimId)
where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>), (<*>))

import AST as A (Primop (..))
import PrimOp as G

-- GHC Wired-in things come from these modules
import TysWiredIn  as G
import TysPrim     as G
import PrelNames   as G
import MkCore      as G
import MkId        as G

import TypeRep (TyThing (..))
import ConLike (ConLike (..))

import Var (Id, idDetails, isId)
import IdInfo (IdDetails (..))
import Name as G ( Name, NamedThing (..), BuiltInSyntax(..)
                 , mkWiredInName, getOccString)
import Class (Class (..))       
import OccName (mkDataOccFS)
import BasicTypes (TupleSort (..))
import FastString (fsLit)
import Constants (mAX_TUPLE_SIZE{-62-}) --  Maybe use an APPFL constant?


lookupAppflPrimop :: G.PrimOp -> Maybe A.Primop
lookupAppflPrimop = (`Map.lookup` primopMap)


primopMap :: Map G.PrimOp A.Primop
primopMap = Map.fromList
  [ (CharGtOp   , Pigt)
  , (CharGeOp   , Pige)
  , (CharEqOp   , Pieq)
  , (CharNeOp   , Pine)
  , (CharLeOp   , Pile)
  , (CharLtOp   , Pilt)    
  , (IntAddOp   , Piadd)
  , (IntSubOp   , Pisub)
  , (IntMulOp   , Pimul)
  , (IntQuotOp  , Pidiv) -- round to zero
  , (IntRemOp   , Pimod)
  , (IntGtOp    , Pigt)
  , (IntGeOp    , Pige)
  , (IntEqOp    , Pieq)
  , (IntNeOp    , Pine)
  , (IntLeOp    , Pile)
  , (IntLtOp    , Pilt)
  , (IntNegOp   , Pineg)   
  , (WordGtOp   , Piadd)
  , (WordGeOp   , Pisub)
  , (WordEqOp   , Pimul)
  , (WordNeOp   , Pidiv)
  , (WordLeOp   , Pimod)
  , (WordLtOp   , Pigt) 
  , (WordAddOp  , Pige) 
  , (WordSubOp  , Pieq) 
  , (WordMulOp  , Pine) 
  , (WordQuotOp , Pile) 
  , (WordRemOp  , Pilt)
--  , (RaiseOp    , Pexcept)
--
--  I want to figure out a way to get this in eventually, even in a
--  limited form that maybe only accepts String literals (as MachStr)
--  Having some kind of user-defined erroring procedure strikes me as
--  particularly useful. - dmr
  ]


-- Some PrimOps are implemented in terms of others in our Base
-- libraries.  This may not warrant its own Map, given how few there
-- may actually be, but at least this explicitly catalogues the ones
-- FromGHC knows about.
lookupImplementedPrimop :: G.PrimOp -> Maybe AppflName
lookupImplementedPrimop = (`Map.lookup` implementedOpMap)

implementedOpMap :: Map G.PrimOp AppflName
implementedOpMap = Map.fromList
  [ (IntQuotRemOp, primDot "quotRemInt#")
  ]


type AppflName = String

-- Names we want to recognize as special.
isAppflBuiltIn :: String -> Bool
isAppflBuiltIn = (`elem` appflBuiltIns)

appflBuiltIns :: [AppflName]
appflBuiltIns = [appflMain, appflPrimIntTy, appflNoExhaust]


-- | Our program entry point
appflMain :: AppflName
appflMain = "main"
-- There's a problem here that I don't have an immediate solution to.  If some
-- Haskell input to our program defines multiple main functions in separate
-- modules, they will all be caught as the _special_ main and named "main",
-- causing errors in DupCheck.
--
-- The potential solutions are:
--  * keep the current behavior and accept a flag (like GHC's -main-is) to
--    disambiguate where necessary
--  * look for a main in the file given as an argument and map _only_ that to
--    the real main
--  * Combine both of the above, overriding the second if a flag is given (Ideal!)
--  * Keep the current behavior and make "Single Main" another constraint on the
--    input.


-- | Our Int#
appflPrimIntTy :: AppflName
appflPrimIntTy = "Int_h"

-- | Our pattern match fail function (defined in the runtime)
appflNoExhaust :: AppflName
appflNoExhaust = "stg_case_not_exhaustive"



isErrorId :: NamedThing a => a -> Bool
isErrorId thing =
  let name = getName thing
  in any ((name ==) . getName) patErrorIDs


namedThingToAppflName :: NamedThing a => a -> Maybe AppflName
namedThingToAppflName thing = Map.lookup (getName thing) builtInMap


idToAppflName :: Id -> Maybe AppflName
idToAppflName id
  | isId id = case idDetails id of
      ClassOpId clas ->
        maybeGetAppflClass (getName clas) (getOccString id)
        -- or just do the normal thing if the lookup fails
        <|> namedThingToAppflName id

      _ -> namedThingToAppflName id
      
  | otherwise = namedThingToAppflName id
        

-- | For some builtin class dictionary selector (e.g. GHC.Classes.==), maybe get
-- the APPFL equivalent.  This assumes that the equivalent class has identical
-- method names, which we need to enforce in our APPFL Base libs.
maybeGetAppflClass clasName selector =
  -- Maybe get the function that prefixes the Module name
  -- and apply it to the selector name if present
  fmap ($ selector) (Map.lookup clasName appflClassModMap)



appflClassModMap :: Map Name (AppflName -> AppflName)
appflClassModMap = Map.fromList
  [ (eqClassName, classesDot)
  , (ordClassName, classesDot) ]

appflClassMap =
  Map.mapWithKey makeClassName appflClassModMap
  where
    makeClassName clasName modPfx = modPfx (getOccString clasName)
      

getClassFromAppflEquiv :: AppflName -> Maybe Name
getClassFromAppflEquiv name = Map.lookup name appflClassImplMap

appflClassImplMap :: Map AppflName G.Name
appflClassImplMap = Map.fromList
  [ (classesDot "Eq", eqClassName) ]

    
    
infixr 5 \.

(\.) :: AppflName -> AppflName -> AppflName
a \. b = a ++ '.' : b

appflDot, typesDot, tupleDot, primDot :: AppflName -> AppflName  
appflDot   s = "APPFL" \. s
typesDot   s = appflDot "Types"   \. s
tupleDot   s = appflDot "Tuple"   \. s
primDot    s = appflDot "Prim"    \. s
classesDot s = appflDot "Classes" \. s


builtInMap :: Map G.Name String
builtInMap =
  -- It should be OK to put all kinds of Names in this map.  GHC's
  -- disambiguation strategies seem to eliminate the chances of any naming
  -- overlap in builtins, even between the different NameSpaces.
  Map.fromList
  [
  --   (G.consDataConName, typesDot "Cons")
  -- , (G.nilDataConName, typesDot "Nil")
  -- , (G.listTyConName, typesDot "List")
  -- , (G.boolTyConName, typesDot "Bool")
  
  -- , (getName G.trueDataCon, typesDot "True")
  -- , (getName G.falseDataCon, typesDot "False")
  -- , (getName G.trueDataConId, typesDot "True")
  -- , (getName G.falseDataConId, typesDot "False")
  -- , (G.intTyConName, typesDot "Int")
  -- , (intDataConName, typesDot "I#")
    
      (getName G.intPrimTyCon, appflPrimIntTy) -- hacky...

    -- GHC generates functions that are only ever passed 'void#', which *seem*
    -- to never use it. In theory, any argument should work, as long as it's
    -- consistent (and not bottom, for strict evals).
  , (getName voidPrimId, primDot "void#") 
  ]
  -- `Map.union` tupleMap
  `Map.union` errorCallMap


-- Not exported from TysWiredIn, aggravating
intDataConName :: Name
intDataConName = mkWiredInName G.gHC_TYPES
                 (mkDataOccFS $ fsLit "I#") G.intDataConKey
                 (AConLike (RealDataCon G.intDataCon)) UserSyntax
                 

tupleMap :: Map G.Name AppflName
tupleMap = Map.fromList $ concat
  [ [ (getName (tupleCon BoxedTuple i)     , tupleDot ("TP" ++ show i))
    , (getName (tupleCon UnboxedTuple i)   , tupleDot ("UTP" ++ show i))
    , (getName (tupleTyCon BoxedTuple i)   , tupleDot ("TP" ++ show i))
    , (getName (tupleTyCon UnboxedTuple i) , tupleDot ("UTP" ++ show i))
    ]
   | i <- [0..mAX_TUPLE_SIZE]]


errorCallMap :: Map G.Name AppflName
errorCallMap = Map.fromList $
               map (\ident -> (getName ident , appflNoExhaust))
               patErrorIDs

patErrorIDs :: [Id]
patErrorIDs =
  [ pAT_ERROR_ID
  , nON_EXHAUSTIVE_GUARDS_ERROR_ID
  , iRREFUT_PAT_ERROR_ID
  ]





{-# DEPRECATED genTupleModule "We don't need a tuple module like GHC's GHC.Tuple" #-}
-- | Generate the APPFL.Tuple module (wherever you'd like).
genTupleModule :: FilePath  -- | Location to write the file to
               -> Maybe Int -- | Maybe you want to specify a max tuple size
               -> IO ()
genTupleModule outFileName m_size =
  writeFile outFileName fileString
  where
    maxSize = fromMaybe mAX_TUPLE_SIZE m_size
    fileString = unlines 
                 $ "{-# LANGUAGE MagicHash #-}" 
                 : "module APPFL.Tuple where" 
                 : "\n\n------ Boxed Tuples ------\n\n" 
                 : mkTupls "TP"
                 ++ "\n\n------ Unboxed Tuples ------\n\n" 
                 : mkTupls "UTP"
    mkTupls con = 
      [let constr = con ++ show i
           tyvars = concatMap (' ':) $ take i allNameStrings
       in "data " ++ constr ++  tyvars ++ "\n   = " ++  constr ++ tyvars
      | i <- [0..maxSize]]
      
      
     
  





--- No code below this line

{----- Unimplemented Primitive Ops
(or implemented in terms of others as Haskell functions in APPFL.Prim)


-------------------- Int Ops --------------------
   | IntMulMayOfloOp
    Return non-zero if there is any possibility that the upper word of a
    signed integer multiply might contain useful information.  Return
    zero only if you are completely sure that no overflow can occur.
    On a 32-bit platform, the recommmended implementation is to do a
    32 x 32 -> 64 signed multiply, and subtract result[63:32] from
    (result[31] >>signed 31).  If this is zero, meaning that the
    upper word is merely a sign extension of the lower one, no
    overflow can occur.

    On a 64-bit platform it is not always possible to
    acquire the top 64 bits of the result.  Therefore, a recommended
    implementation is to take the absolute value of both operands, and
    return 0 iff bits[63:31] of them are zero, since that means that their
    magnitudes fit within 31 bits, so the magnitude of the product must fit
    into 62 bits.

    If in doubt, return non-zero, but do make an effort to create the
    correct answer for small args, since otherwise the performance of
    (*) :: Integer -> Integer -> Integer will be poor.


   | AndIOp
   | OrIOp
   | XorIOp
   | NotIOp
   | IntAddCOp  -- i# -> i# -> (# i#, i#  #)
                -- add ints, result (possibly truncated) in first element
                -- report overflow with non-zero val in second element
   | IntSubCOp  -- i# -> i# -> (# i, i#  #)
                -- subtract ints, result (possibly truncated) in first element
                -- report overflow with non-zero val in second element
   | ChrOp
   | Int2WordOp
   | Int2FloatOp
   | Int2DoubleOp
   | ISllOp   -- unchecked int shift left
   | ISraOp   -- unchecked int shift right (arithmetic)
   | ISrlOp   -- unchecked int shift right (logical)


-------------------- Word Ops --------------------
   | Word2FloatOp
   | Word2DoubleOp
   | WordAdd2Op  -- returns (# high, low #)

   | WordQuotRemOp
   | WordMul2Op  -- w# -> w# -> w# -> (# w#, w#  #)
                 -- high word of dividend -> low word of dividend
                 -- -> divisor -> (high, low)
   | WordQuotRem2Op
   | AndOp
   | OrOp
   | XorOp
   | NotOp
   | SllOp  -- unchecked word shift left
   | SrlOp  -- unchecked word shift right (logical)
   | Word2IntOp
   -- bit twiddling ops on Words
   | PopCnt8Op   -- Hamming weight of low 8 bits
   | PopCnt16Op  -- Hamming weight of low 16 bits
   | PopCnt32Op  -- Hamming weight of low 32 bits
   | PopCnt64Op  -- Hamming weight of 64 bit word
   | PopCntOp    -- Hamming weight of word

   | Clz8Op      -- count leading zeros of low 8 bits
   | Clz16Op     -- count leading zeros of low 16 bits
   | Clz32Op     -- count leading zeros of low 32 bits
   | Clz64Op     -- count leading zeros of 64 bit word
   | ClzOp       -- count leading zeros of word

   | Ctz8Op      -- count trailing zeros of low 8 bits 
   | Ctz16Op     -- count trailing zeros of low 16 bits
   | Ctz32Op     -- count trailing zeros of low 32 bits
   | Ctz64Op     -- count trailing zeros of 64 bit word
   | CtzOp       -- count trailing zeros of word       

   | BSwap16Op   -- Swap two low bytes
   | BSwap32Op   -- Swap four low bytes (i.e. reverse)
   | BSwap64Op   -- Swap eight low bytes (i.e. reverse)
   | BSwapOp     -- Swap bytes (i.e. reverse)

   -- narrowing ops on ints (truncation?)
   | Narrow8IntOp
   | Narrow16IntOp
   | Narrow32IntOp
   | Narrow8WordOp
   | Narrow16WordOp
   | Narrow32WordOp

   | DoubleGtOp
   | DoubleGeOp
   | DoubleEqOp
   | DoubleNeOp
   | DoubleLtOp
   | DoubleLeOp
   | DoubleAddOp
   | DoubleSubOp
   | DoubleMulOp
   | DoubleDivOp
   | DoubleNegOp
   | Double2IntOp
   | Double2FloatOp
   | DoubleExpOp
   | DoubleLogOp
   | DoubleSqrtOp
   | DoubleSinOp
   | DoubleCosOp
   | DoubleTanOp
   | DoubleAsinOp
   | DoubleAcosOp
   | DoubleAtanOp
   | DoubleSinhOp
   | DoubleCoshOp
   | DoubleTanhOp
   | DoublePowerOp
   | DoubleDecode_2IntOp
   | DoubleDecode_Int64Op


   | FloatGtOp
   | FloatGeOp
   | FloatEqOp
   | FloatNeOp
   | FloatLtOp
   | FloatLeOp
   | FloatAddOp
   | FloatSubOp
   | FloatMulOp
   | FloatDivOp
   | FloatNegOp
   | Float2IntOp
   | FloatExpOp
   | FloatLogOp
   | FloatSqrtOp
   | FloatSinOp
   | FloatCosOp
   | FloatTanOp
   | FloatAsinOp
   | FloatAcosOp
   | FloatAtanOp
   | FloatSinhOp
   | FloatCoshOp
   | FloatTanhOp
   | FloatPowerOp
   | Float2DoubleOp
   | FloatDecode_IntOp

------------------------------------------------------------
Below here are ops I doubt we'll ever add, unless to do something
with Strings for a primitive error function, maybe?
 

   | NewArrayOp
   | SameMutableArrayOp
   | ReadArrayOp
   | WriteArrayOp
   | SizeofArrayOp
   | SizeofMutableArrayOp
   | IndexArrayOp
   | UnsafeFreezeArrayOp
   | UnsafeThawArrayOp
   | CopyArrayOp
   | CopyMutableArrayOp
   | CloneArrayOp
   | CloneMutableArrayOp
   | FreezeArrayOp
   | ThawArrayOp
   | CasArrayOp
   | NewSmallArrayOp
   | SameSmallMutableArrayOp
   | ReadSmallArrayOp
   | WriteSmallArrayOp
   | SizeofSmallArrayOp
   | SizeofSmallMutableArrayOp
   | IndexSmallArrayOp
   | UnsafeFreezeSmallArrayOp
   | UnsafeThawSmallArrayOp
   | CopySmallArrayOp
   | CopySmallMutableArrayOp
   | CloneSmallArrayOp
   | CloneSmallMutableArrayOp
   | FreezeSmallArrayOp
   | ThawSmallArrayOp
   | CasSmallArrayOp
   | NewByteArrayOp_Char
   | NewPinnedByteArrayOp_Char
   | NewAlignedPinnedByteArrayOp_Char
   | ByteArrayContents_Char
   | SameMutableByteArrayOp
   | ShrinkMutableByteArrayOp_Char
   | ResizeMutableByteArrayOp_Char
   | UnsafeFreezeByteArrayOp
   | SizeofByteArrayOp
   | SizeofMutableByteArrayOp
   | IndexByteArrayOp_Char
   | IndexByteArrayOp_WideChar
   | IndexByteArrayOp_Int
   | IndexByteArrayOp_Word
   | IndexByteArrayOp_Addr
   | IndexByteArrayOp_Float
   | IndexByteArrayOp_Double
   | IndexByteArrayOp_StablePtr
   | IndexByteArrayOp_Int8
   | IndexByteArrayOp_Int16
   | IndexByteArrayOp_Int32
   | IndexByteArrayOp_Int64
   | IndexByteArrayOp_Word8
   | IndexByteArrayOp_Word16
   | IndexByteArrayOp_Word32
   | IndexByteArrayOp_Word64
   | ReadByteArrayOp_Char
   | ReadByteArrayOp_WideChar
   | ReadByteArrayOp_Int
   | ReadByteArrayOp_Word
   | ReadByteArrayOp_Addr
   | ReadByteArrayOp_Float
   | ReadByteArrayOp_Double
   | ReadByteArrayOp_StablePtr
   | ReadByteArrayOp_Int8
   | ReadByteArrayOp_Int16
   | ReadByteArrayOp_Int32
   | ReadByteArrayOp_Int64
   | ReadByteArrayOp_Word8
   | ReadByteArrayOp_Word16
   | ReadByteArrayOp_Word32
   | ReadByteArrayOp_Word64
   | WriteByteArrayOp_Char
   | WriteByteArrayOp_WideChar
   | WriteByteArrayOp_Int
   | WriteByteArrayOp_Word
   | WriteByteArrayOp_Addr
   | WriteByteArrayOp_Float
   | WriteByteArrayOp_Double
   | WriteByteArrayOp_StablePtr
   | WriteByteArrayOp_Int8
   | WriteByteArrayOp_Int16
   | WriteByteArrayOp_Int32
   | WriteByteArrayOp_Int64
   | WriteByteArrayOp_Word8
   | WriteByteArrayOp_Word16
   | WriteByteArrayOp_Word32
   | WriteByteArrayOp_Word64
   | CopyByteArrayOp
   | CopyMutableByteArrayOp
   | CopyByteArrayToAddrOp
   | CopyMutableByteArrayToAddrOp
   | CopyAddrToByteArrayOp
   | SetByteArrayOp
   | AtomicReadByteArrayOp_Int
   | AtomicWriteByteArrayOp_Int
   | CasByteArrayOp_Int
   | FetchAddByteArrayOp_Int
   | FetchSubByteArrayOp_Int
   | FetchAndByteArrayOp_Int
   | FetchNandByteArrayOp_Int
   | FetchOrByteArrayOp_Int
   | FetchXorByteArrayOp_Int
   | NewArrayArrayOp
   | SameMutableArrayArrayOp
   | UnsafeFreezeArrayArrayOp
   | SizeofArrayArrayOp
   | SizeofMutableArrayArrayOp
   | IndexArrayArrayOp_ByteArray
   | IndexArrayArrayOp_ArrayArray
   | ReadArrayArrayOp_ByteArray
   | ReadArrayArrayOp_MutableByteArray
   | ReadArrayArrayOp_ArrayArray
   | ReadArrayArrayOp_MutableArrayArray
   | WriteArrayArrayOp_ByteArray
   | WriteArrayArrayOp_MutableByteArray
   | WriteArrayArrayOp_ArrayArray
   | WriteArrayArrayOp_MutableArrayArray
   | CopyArrayArrayOp
   | CopyMutableArrayArrayOp
   | AddrAddOp
   | AddrSubOp
   | AddrRemOp
   | Addr2IntOp
   | Int2AddrOp
   | AddrGtOp
   | AddrGeOp
   | AddrEqOp
   | AddrNeOp
   | AddrLtOp
   | AddrLeOp
   | IndexOffAddrOp_Char
   | IndexOffAddrOp_WideChar
   | IndexOffAddrOp_Int
   | IndexOffAddrOp_Word
   | IndexOffAddrOp_Addr
   | IndexOffAddrOp_Float
   | IndexOffAddrOp_Double
   | IndexOffAddrOp_StablePtr
   | IndexOffAddrOp_Int8
   | IndexOffAddrOp_Int16
   | IndexOffAddrOp_Int32
   | IndexOffAddrOp_Int64
   | IndexOffAddrOp_Word8
   | IndexOffAddrOp_Word16
   | IndexOffAddrOp_Word32
   | IndexOffAddrOp_Word64
   | ReadOffAddrOp_Char
   | ReadOffAddrOp_WideChar
   | ReadOffAddrOp_Int
   | ReadOffAddrOp_Word
   | ReadOffAddrOp_Addr
   | ReadOffAddrOp_Float
   | ReadOffAddrOp_Double
   | ReadOffAddrOp_StablePtr
   | ReadOffAddrOp_Int8
   | ReadOffAddrOp_Int16
   | ReadOffAddrOp_Int32
   | ReadOffAddrOp_Int64
   | ReadOffAddrOp_Word8
   | ReadOffAddrOp_Word16
   | ReadOffAddrOp_Word32
   | ReadOffAddrOp_Word64
   | WriteOffAddrOp_Char
   | WriteOffAddrOp_WideChar
   | WriteOffAddrOp_Int
   | WriteOffAddrOp_Word
   | WriteOffAddrOp_Addr
   | WriteOffAddrOp_Float
   | WriteOffAddrOp_Double
   | WriteOffAddrOp_StablePtr
   | WriteOffAddrOp_Int8
   | WriteOffAddrOp_Int16
   | WriteOffAddrOp_Int32
   | WriteOffAddrOp_Int64
   | WriteOffAddrOp_Word8
   | WriteOffAddrOp_Word16
   | WriteOffAddrOp_Word32
   | WriteOffAddrOp_Word64
   | NewMutVarOp
   | ReadMutVarOp
   | WriteMutVarOp
   | SameMutVarOp
   | AtomicModifyMutVarOp
   | CasMutVarOp
   | CatchOp
   | RaiseIOOp
   | MaskAsyncExceptionsOp
   | MaskUninterruptibleOp
   | UnmaskAsyncExceptionsOp
   | MaskStatus
   | AtomicallyOp
   | RetryOp
   | CatchRetryOp
   | CatchSTMOp
   | Check
   | NewTVarOp
   | ReadTVarOp
   | ReadTVarIOOp
   | WriteTVarOp
   | SameTVarOp
   | NewMVarOp
   | TakeMVarOp
   | TryTakeMVarOp
   | PutMVarOp
   | TryPutMVarOp
   | ReadMVarOp
   | TryReadMVarOp
   | SameMVarOp
   | IsEmptyMVarOp
   | DelayOp
   | WaitReadOp
   | WaitWriteOp
   | ForkOp
   | ForkOnOp
   | KillThreadOp
   | YieldOp
   | MyThreadIdOp
   | LabelThreadOp
   | IsCurrentThreadBoundOp
   | NoDuplicateOp
   | ThreadStatusOp
   | MkWeakOp
   | MkWeakNoFinalizerOp
   | AddCFinalizerToWeakOp
   | DeRefWeakOp
   | FinalizeWeakOp
   | TouchOp
   | MakeStablePtrOp
   | DeRefStablePtrOp
   | EqStablePtrOp
   | MakeStableNameOp
   | EqStableNameOp
   | StableNameToIntOp
   | ReallyUnsafePtrEqualityOp
   | ParOp
   | SparkOp
   | SeqOp
   | GetSparkOp
   | NumSparks
   | ParGlobalOp
   | ParLocalOp
   | ParAtOp
   | ParAtAbsOp
   | ParAtRelOp
   | ParAtForNowOp
   | DataToTagOp
   | TagToEnumOp
   | AddrToAnyOp
   | MkApUpd0_Op
   | NewBCOOp
   | UnpackClosureOp
   | GetApStackValOp
   | GetCCSOfOp
   | GetCurrentCCSOp
   | TraceEventOp
   | TraceMarkerOp
   | VecBroadcastOp PrimOpVecCat Length Width
   | VecPackOp PrimOpVecCat Length Width
   | VecUnpackOp PrimOpVecCat Length Width
   | VecInsertOp PrimOpVecCat Length Width
   | VecAddOp PrimOpVecCat Length Width
   | VecSubOp PrimOpVecCat Length Width
   | VecMulOp PrimOpVecCat Length Width
   | VecDivOp PrimOpVecCat Length Width
   | VecQuotOp PrimOpVecCat Length Width
   | VecRemOp PrimOpVecCat Length Width
   | VecNegOp PrimOpVecCat Length Width
   | VecIndexByteArrayOp PrimOpVecCat Length Width
   | VecReadByteArrayOp PrimOpVecCat Length Width
   | VecWriteByteArrayOp PrimOpVecCat Length Width
   | VecIndexOffAddrOp PrimOpVecCat Length Width
   | VecReadOffAddrOp PrimOpVecCat Length Width
   | VecWriteOffAddrOp PrimOpVecCat Length Width
   | VecIndexScalarByteArrayOp PrimOpVecCat Length Width
   | VecReadScalarByteArrayOp PrimOpVecCat Length Width
   | VecWriteScalarByteArrayOp PrimOpVecCat Length Width
   | VecIndexScalarOffAddrOp PrimOpVecCat Length Width
   | VecReadScalarOffAddrOp PrimOpVecCat Length Width
   | VecWriteScalarOffAddrOp PrimOpVecCat Length Width
   | PrefetchByteArrayOp3
   | PrefetchMutableByteArrayOp3
   | PrefetchAddrOp3
   | PrefetchValueOp3
   | PrefetchByteArrayOp2
   | PrefetchMutableByteArrayOp2
   | PrefetchAddrOp2
   | PrefetchValueOp2
   | PrefetchByteArrayOp1
   | PrefetchMutableByteArrayOp1
   | PrefetchAddrOp1
   | PrefetchValueOp1
   | PrefetchByteArrayOp0
   | PrefetchMutableByteArrayOp0
   | PrefetchAddrOp0
   | PrefetchValueOp0
-}
