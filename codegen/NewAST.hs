
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE TypeSynonymInstances     #-}

module NewAST
(
  Pattern (..),
  Defn (..),
  Constr (..),
  Exp (..),
  Primop (..),
  Atom (..),
  Monotype (..),
  isODefn,
  isTDefn,
  isDDefn,
  partitionDefs,
)
where

import AST
import ADT
import Data.List (nubBy, partition)
import Data.Function (on)
import PPrint
import Util (mapSnd)


data Defn = ODefn -- object definition 
            {bnd :: Var,  -- variable bound to
             oexp :: Exp, -- expression being bound
             mmtype :: Maybe Monotype} -- Type, if signature present
            
          | DDefn -- datatype definition
            {mtyp :: Monotype,  -- MCon holding Type info (name, boxed, pvars)
             dcons :: [Constr]} -- List of constructors
            
          | TDefn -- type signature definition
            {bnd :: Var,  -- variable to give type information for
             mtyp :: Monotype} -- Type specified
          deriving (Show,Eq)

partitionDefs defs =
  let (ds, rs) = partition isDDefn defs
      (os, ts) = partition isODefn rs
  in (ds, os, ts)

isODefn d = case d of
  ODefn{} -> True
  _       -> False

isDDefn d = case d of
  DDefn{} -> True
  _       -> False

isTDefn d = case d of
  TDefn{} -> True
  _       -> False  


data Constr = DCon {dcon :: Con, mtyps :: [Monotype]}
            deriving (Show,Eq)

data Exp = EAp {fexp :: Exp, eexp :: Exp}
         | EFn {pats :: [Pattern], eexp :: Exp}
         | ECs {eexp :: Exp, cls :: [Clause]}
         | ELt {eexp :: Exp, defns :: [Defn]}
         | EAt {atm :: Atom}
           deriving (Show,Eq)

type Clause = (Pattern, Exp)


data Pattern = Match {str :: String, npats :: [Pattern]}
             | Default {str :: String}
             deriving (Show,Eq)

isDefaultPat p = case p of
  Default{} -> True
  _ -> False

















{- Building from _The Implementation of Functional Programming Languages_
 Ch. 5 - Efficient Compilation of Pattern-matching (written by Phillip Wadler)

given functions with pattern matching

f p_1-1 .. p_1-n = e_1
.
.
f p_m-1 .. p_m-n = e_m

-}

match ::
  [Var] -> -- list of arguments to the function (probably generated)
  [([Pattern], Exp)] -> -- Pattern : Expression pairs
  Exp -> -- default expression (possibly error-type)
  Expr () -- equivalent expression in STG AST form
match vars prs dflt =
  case vars of
   [] -> undefined
   v:xs ->
     let prs' = rmHidden prs
         grouped = groupAllBy ((==) `on` (str.head.fst)) prs'
         fsts = map (head.fst.head) grouped
         subs = map ((,v) . str) $ filter isDefaultPat fsts
         subbed = map (map (\(p,e) -> (p, subNamesEx subs e))) grouped
     in undefined
     where
       rmHidden = foldr (\(ps1,_) pps -> filter (\(ps2,_) -> or $ zipWith hides ps1 ps2) pps) []
       hides Default{} _ = True
       hides _ Default{} = False
       hides (Match c1 ps1) (Match c2 ps2) = c1 == c2 && and (zipWith hides ps1 ps2)
  

                
-- first-occurrence-order-preserving grouping of similar elements in a list
groupAllBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupAllBy f xs =
  let nubbed = nubBy f xs
  in if (length nubbed == length xs)
-- if list is full of uniques, package them in lists
     then map (:[]) xs 
-- else fold on the uniques, accumulating the groups (hence reverse) and the unsorted
     else reverse $ fst $ foldl ffun ([],xs) nubbed
  where ffun (groups, xs) it = let (matches, rest) = partition (f it) xs
                               in (matches:groups, rest)





subNamesEx :: [(Var,Var)] -> -- replace fst with snd
            Exp -> -- in this expression
            Exp -- to produce this expression
subNamesEx ls e = case e of
  EAt{atm} ->
    case atm of
     Var v -> case lookup v ls of
               Nothing -> e
               Just v' -> e{atm = Var v'}
     _ -> e -- Literal
     
  EAp{fexp, eexp} -> e { fexp = subNamesEx ls fexp,
                          eexp = subNamesEx ls eexp }

  ECs{eexp, cls} ->
    let newBinds = concatMap (patBinds . fst) cls
        ls'      = filter ((`elem` newBinds) . fst) ls
        eexp'    = subNamesEx ls' eexp
        cls'     = map (mapSnd (subNamesEx ls')) cls
    in  e { eexp = eexp',
            cls  = cls'}

  ELt{eexp, defns} ->
    let newBinds = map bnd defns
        ls' = filter ((`elem` newBinds) . fst) ls
    in e { eexp = subNamesEx ls' eexp,
           defns = map (subNamesDf ls') defns }

  EFn{pats, eexp} ->
    let newBinds = concatMap patBinds pats
        ls' = filter ((`elem` newBinds) . fst) ls
    in e { eexp = subNamesEx ls' eexp }

subNamesDf :: [(Var,Var)] -> Defn -> Defn
subNamesDf ls d = case d of
  ODefn{oexp} -> d {oexp = subNamesEx ls oexp}
  _           -> error "ASTz.subNamesDf: should not be passing TDefn or DDefn"

patBinds :: Pattern -> [Var]
patBinds Default{str} = [str]
patBinds Match{str, npats} = str : concatMap patBinds npats




genVars pats env = foldr fun ([], env) pats
  where fun Default{str} (vars, env) = (str:vars, addToEnv str env)
        fun _ (vars, env) =
          let (v, env') = nextFreeInEnv "_gv" env in (v:vars, env')


addToEnv s [] = [(s,0)]
addToEnv s env@(pr@(s',i):xs)
  | s == s' = env
  | otherwise = pr : addToEnv s xs

nextFreeInEnv s [] = (s ++ "0", [(s,1)])
nextFreeInEnv s (pr@(s',i):xs)=
  let (res, env') = if s == s'
                    then (s ++ show i, (s, i+1):xs)
                    else nextFreeInEnv s xs
      -- guarantee no accidental overlap after name is generated
      (res', env'') = if res == s'
                      then nextFreeInEnv s env'
                      else (res,env')
  in (res', pr:env'')


type Assoc a b = [(a,b)]

delete :: Eq a => a -> Assoc a b -> Assoc a b
delete k assoc = foldr fun [] assoc
  where fun e@(k',_) d | k == k' = d
                       | otherwise = e:d
                                       
insertWith :: (Eq a) => (b -> b -> b) -> a -> b -> Assoc a b -> Assoc a b
insertWith f k v assoc | k `inAssoc` assoc = foldr fun [] assoc
                      | otherwise = (k,v):assoc
  where fun e@(k',v') d | k == k' = (k, f v v'):d
                        | otherwise = e:d

insert :: (Eq a) => a -> b -> Assoc a b -> Assoc a b
insert = insertWith const

inAssoc :: (Eq a) => a -> Assoc a b -> Bool
inAssoc k assoc = any (== k) $ map fst assoc

lookUpBy :: (a -> a -> Bool) -> a -> Assoc a b -> Maybe b
lookUpBy f k assoc = case filter ((f k) . fst) assoc of
  []      -> Nothing
  (_,v):_ -> Just v



instance Unparse (Defn) where
  unparse TDefn{bnd, mtyp} =
    text bnd <+> doubleColon <+> unparse mtyp <> semi

  unparse DDefn{mtyp, dcons} =
    case mtyp of
        MCon b c ms ->
          let lh = text "data" <+>
                   (if b then empty else text "unboxed") <+>
                   hsep (map unparse ms) <+> -- should only be MVar
                   equals
              (d:ds) = dcons -- should never have fewer than 1 constructor
              sepr = bar <> text " "
              ind = (length $ show lh) + 1
              rh = nest ind (unparse d $$
                   nest (-2) (vcat $ prepunctuate sepr $ map unparse ds))
          in lh $$ rh <> semi
                    
        m -> error $ "DDefn holds non-MCon defining monotype: " ++ show m

  unparse ODefn{bnd, oexp} =
    text bnd <+>
    case oexp of
     EFn{pats, eexp} -> hsep (map unparse pats) <+>
                        equals <+>
                        unparse eexp <>
                        semi
     _               -> equals <+> unparse oexp <> semi



instance Unparse (Constr) where
  unparse DCon{dcon, mtyps} =
    text dcon <+> hcat (map unparse mtyps)

instance Unparse (Exp) where
  unparse EAp{fexp, eexp} =
    let fd = case eexp of
              EAt{atm} -> unparse atm
              _ -> parens (unparse eexp)
    in unparse fexp <+> fd

  unparse EFn{pats, eexp} =
    parens $
    lambda <+> hsep (map unparse pats) <+>
    arw <+> unparse eexp

  unparse ECs{eexp, cls} =
    text "case" <+> unparse eexp <+> text "of" $+$
    nest 2 (vcat $ map unparse cls)

  unparse ELt{eexp, defns} =
    text "let" $+$
    nest 2 (vcat $ map unparse defns) $+$
    text "in" <+> unparse eexp

  unparse EAt{atm} = unparse atm

instance Unparse (Clause) where
  unparse (p,e) = unparse p <+> arw <+> unparse e <> semi

instance Unparse Pattern where
  unparse Match{str, npats} =
    case npats of
     [] -> text str
     _  -> parens (text str <+> hsep (map unparse npats))

  unparse Default{str} = text str

