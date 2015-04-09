{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}

module Parser (
  Var,
  Con,
  Atom(..),
  Expr(..),
  Alt(..),
  Alts(..),
  Obj(..),
  parser,
  -- showDefs,
) where

import Lexer
import ParseComb
import Data.List

{-  grammar

<var> :: C syntax, more or less

<con> :: start with uppercase

<lit> ::= int[eger]

<atom> ::= <lit> | <var>

<prog> ::= <def> (";" <def>)*

<obj> ::= "FUN" "(" <var>+ -> <expr> ")"
       |  "PAP" "(" <var> <atom>+ ")"
       |  "CON" "(" <con> <atom>* ")"
       |  "THUNK" <expr>
       |  "ERROR"  (aka BLACKHOLE)

<expr> ::= <atom>
       |  <var>"^"<arity> <atom>+
       |  <primop> <atom>+
       |  "let" "{" <defs> "}" "in" <expr>
       |  "case" <expr> "of" "{" <alts> "}"

<alts> ::= <alt> (";" <alt>)*

<alt> ::= <con> <var>* "->" <expr>
       |  <var> "->" <expr>

<arity> ::= <pos> | "_"

-}

-- in the spirit of intercalate

precalate s [] = []
precalate s (s':ss) = s ++ s' ++ precalate s ss

dropspaces = dropWhile (==' ')

interpolate ('%':'%':s) = interpolate $ '%':s
interpolate ('%':'\n':'%':'%':s) = interpolate $ '%':'\n':'%':s
interpolate ('%':'\n':'%':s) = interpolate $ dropspaces s
interpolate ('%':'\n':s) = interpolate $ dropspaces s
interpolate ('\n':'%':s) = interpolate $ dropspaces s
interpolate (c:s) = c : interpolate s
interpolate [] = []

showDefs defs = interpolate $ intercalate "\n" $ unparser 0 defs

indent n s@('%':_) = s
indent n s = (take n $ repeat ' ') ++ s

indents n ss = map (indent n) ss

-- instance Unparser n Atom where
showa (Var v) = v
showa (Lit l) = show l

-- instance Unparser [Atom] where
showas as = intercalate " " $ map showa as

showFVs vs = "[" ++ intercalate " " vs ++ "] "

class Unparser a where unparser :: Int -> a -> [String]

instance Unparser (Expr [Var]) where
    unparser n (EAtom fvs a) = 
        indents n [showFVs fvs ++ showa a]

    unparser n (EFCall fvs f as) = 
        indents n [showFVs fvs ++ f ++ " " ++ showas as]

    unparser n (EPrimop fvs p as) = 
        indents n [showFVs fvs ++ "PRIMOP " ++ showas as]

    unparser n (ELet fvs defs e) = 
        let ss = [showFVs fvs ++ "let { %"] ++
                 unparser 6 defs ++
                 ["} in %"] ++
                 unparser 5 e
        in indents n ss

    unparser n (ECase fvs e alts) = 
        let ss = [showFVs fvs ++ "case %"] ++ 
                 unparser 5 e ++
                 ["of { %"] ++
                 unparser 5 alts ++
                 ["%}"]
        in indents n ss

instance Unparser (Alt [Var]) where
    unparser n (ACon fvs c vs e) = 
        let line = showFVs fvs ++ c ++ precalate " " vs ++ " -> %"
            ss = [line] ++
                 unparser (length line - 1) e
        in indents n ss

    unparser n (ADef fvs v e) = 
        let ss = [showFVs fvs ++ v ++ " -> %"] ++
                 unparser (4 + length v) e
        in indents n ss

instance Unparser (Alts [Var]) where
    unparser n (Alts {alts}) = 
        concatMap (unparser n) alts

instance Unparser (Obj [Var]) where
    unparser n (FUN fvs vs e _) = 
        let ss = [showFVs fvs ++ "FUN( " ++ intercalate " " vs ++ " ->"] ++
                 unparser 2 e ++
                 ["%)"]
        in indents n ss

    unparser n (PAP fvs f as _) = 
        let ss = [showFVs fvs ++ "PAP( " ++ f ++ " " ++ showas as ++ " )"]
        in indents n ss

    unparser n (CON fvs c as _) = 
        let ss = [showFVs fvs ++ "CON( " ++ c ++ " " ++ showas as ++ " )"]
        in indents n ss

    unparser n (THUNK fvs e _) = 
        let ss = [showFVs fvs ++ "THUNK( %"] ++
                 unparser 7 e ++
                 ["% )"]
        in indents n ss

    unparser n (BLACKHOLE _ _) =
        indents n ["BLACKHOLE"]

instance Unparser [Obj [Var]] where
    -- unparser n defs = intercalate ["\n"] $ map (unparser n) defs
    unparser n defs = concatMap (unparserdef n) defs

unparserdef n o =
    let ss = [ oname o ++ " = %" ] ++
             unparser 2 o
    in indents n ss

-- Parser

type Var = String
type Con = String

data Atom = Var Var
          | Lit Int
            deriving(Eq,Show)

data Expr a = EAtom   {emd :: a, ea :: Atom}
            | EFCall  {emd :: a, ev :: Var, eas :: [Atom]}
            | EPrimop {emd :: a, eprimop :: Primop, eas :: [Atom]}
            | ELet    {emd :: a, edefs :: [Obj a], ee :: Expr a}
            | ECase   {emd :: a, ee :: Expr a, ealts :: Alts a}
              deriving(Eq,Show)

data Alt a = ACon {amd :: a, ac :: Con, avs :: [Var], ae :: Expr a}
           | ADef {amd :: a,            av :: Var,    ae :: Expr a}
             deriving(Eq,Show)

data Alts a = Alts {altsmd :: a, alts :: [Alt a], aname :: String}
              deriving(Eq,Show)

data Obj a = FUN   {omd :: a, vs :: [Var],   e :: (Expr a), oname :: String}
           | PAP   {omd :: a, f  :: Var,     as :: [Atom],  oname :: String}
           | CON   {omd :: a, c  :: Con,     as :: [Atom],  oname :: String}
           | THUNK {omd :: a, e  :: (Expr a)             ,  oname :: String}
           | BLACKHOLE {omd :: a                         ,  oname :: String}
             deriving(Eq,Show)

-- type Token = (Tag, [Char])
-- type Parser a b = [a] -> [(b, [a])]

parser :: [Char] -> [Obj ()]
parser inp = case (defsp $ lexer inp) of
               [] ->  error "parser failed"
               xs -> fst $ head xs

primopp ((PO po):xs) = succeedp po xs
primopp _ = failp []

symkindp s1 ((Sym s2):xs) | s1 == s2 = succeedp s1 xs
symkindp _ _ = failp []

objkindp o1 ((Obj o2):xs) | o1 == o2 = succeedp o1 xs
objkindp _ _ = failp []

kwkindp k1 ((KW k2):xs) | k1 == k2 = succeedp k1 xs
kwkindp _ _ = failp []

nump :: Parser Token Int
nump ((Number i) : xs) = succeedp i xs
nump _ = failp []

varp :: Parser Token String
varp ((Ident s) : xs) = succeedp s xs
varp _ = failp []

conp :: Parser Token String
conp ((Ctor c) : xs) = succeedp c xs
conp _ = failp []

atomp :: Parser Token Atom
atomp = (nump `usingp` Lit) `altp` (varp `usingp` Var)

defsp :: Parser Token [Obj ()]
defsp = sepbyp defp (symkindp SymSemi)

defp :: Parser Token (Obj ())
defp = varp `thenp` 
       cutp "defp_1" 
       (symkindp SymBind `xthenp` (cutp "defp_2" objp))
       `usingp` \(v,o) -> o{oname = v}

objp :: Parser Token (Obj ())
objp = funobjp `altp`
       papobjp `altp`
       conobjp `altp`
       thunkobjp `altp`
       blackholeobjp

blackholeobjp :: Parser Token (Obj ())
blackholeobjp = objkindp OBLACKHOLE `usingp` const (BLACKHOLE () "")

thunkobjp = objkindp OTHUNK `xthenp` cutp "thunkobjp_1"
            (symkindp SymLParen `xthenp`  cutp "thunkobjp_2"
             (exprp  `thenxp` cutp "thunkobjp_3"
              (symkindp SymRParen)))
            `usingp` \e ->  THUNK () e ""

conobjp = (objkindp OCON `xthenp` 
          (symkindp SymLParen`xthenp` 
           (conp `thenp` (manyp atomp `thenxp` symkindp SymRParen))))
         `usingp` \(c,as) -> CON () c as ""

funobjp = (objkindp OFUN `xthenp` cutp "funobjp_1"
          (symkindp SymLParen `xthenp` cutp "funobjp_2"
           (somep varp `thenp` cutp "funobjp_3"
            (symkindp SymArrow `xthenp` cutp "funobjp_4"
             (exprp `thenxp` cutp "funobjp_5"
              (symkindp SymRParen))))))
         `usingp` \(f,as) -> FUN () f as ""

papobjp = (objkindp OPAP `xthenp` 
          (symkindp SymLParen `xthenp` 
           (varp `thenp` 
            (somep atomp `thenxp` 
             symkindp SymRParen))))
         `usingp` \(p,as) -> (PAP () p as "")

exprp =        eprimopp   -- preceed efcallp, primops are just distinguished vars
        `altp` efcallp
        `altp` eletp 
        `altp` ecasep
        `altp` eatomp

eatomp = atomp `usingp` (EAtom ())

efcallp = (varp `thenp` somep atomp) 
          `usingp` uncurry (EFCall ())

eprimopp = (primopp `thenp` somep atomp) `usingp` uncurry (EPrimop ())

eletp = (kwkindp KWlet `xthenp` cutp "eletp_1"
         (symkindp SymLBrace `xthenp`  cutp "eletp_2"
          (defsp `thenp`  cutp "eletp_3"       -- [def]
           (symkindp SymRBrace `xthenp`  cutp "eletp_4"
            (kwkindp KWin `xthenp`  cutp "eletp_5"
             exprp)))))         -- expr
        `usingp` uncurry (ELet ())
          
ecasep = (kwkindp KWcase `xthenp` cutp "ecasep_1"                -- case
          (exprp `thenp` cutp "ecasep_2"                          -- expr
           (kwkindp KWof `xthenp`  cutp "ecasep_3"                 -- of
            (symkindp SymLBrace `xthenp`  cutp "ecasep_4"         -- 
             (alternsp `thenxp` cutp "ecasep_5"
              (symkindp SymRBrace)))))) 
         `usingp` uncurry (ECase ())

alternsp = sepbyp alternp (symkindp SymSemi) 
           `usingp` \alts -> Alts () alts "alts"
            
alternp = aconp `altp` adefp

aconp = conp `thenp` 
        (manyp varp `thenp` 
         (symkindp SymArrow `xthenp` 
          exprp)) 
        `usingp` \(c,(vs,e)) -> ACon () c vs e

adefp = (varp `thenp`
         (symkindp SymArrow `xthenp`
          exprp))
        `usingp` uncurry (ADef ())


