module Parser (
  Var,
  Con,
  Atom(..),
  Arity,
  Expr(..),
  Alt(..),
  Obj(..),
  Def,
  Defs,
  parser
) where

import Lexer
import ParseComb

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

-- Parser

type Var = String

type Con = String

type Name = String

-- Maybe Nothing will be filled in later--these are the points where the CG:
-- for expressions, cue the GC to generate a separate function
-- for objects, needs to name infoTab and static heap objects

data Atom = Var Var
          | Lit Int
            deriving(Eq,Show)

type Arity = Maybe Int

data Expr = EAtom Atom
          | EFCall Var Arity [Atom]
          | EPrimop Primop [Atom]
          | ELet Defs (Maybe Name, Expr)
          | ECase (Maybe Name, Expr) [(Maybe Name, Alt)]
            deriving(Eq,Show)

data Alt = ACon Con [Var] Expr
         | ADef Var Expr
           deriving(Eq,Show)

data Obj = FUN Int [Var] Expr
         | PAP Var [Atom]
         | CON Con [Atom]
         | THUNK Expr
         | BLACKHOLE
           deriving(Eq,Show)

type Def = (Var, (Maybe Name, Obj))

type Defs = [Def]


-- type Token = (Tag, [Char])
-- type Parser a b = [a] -> [(b, [a])]

parser :: [Char] -> Defs
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

defsp :: Parser Token [(String, (Maybe Name, Obj))]
defsp = sepbyp defp (symkindp SymSemi)

defp :: Parser Token (String, (Maybe Name, Obj))
defp = varp `thenp` 
       cutp "defp_1" 
       (symkindp SymBind `xthenp` (cutp "defp_2" objp))

namefst x = (Nothing, x)

objp :: Parser Token (Maybe Name, Obj)
objp = funobjp `altp`
       papobjp `altp`
       conobjp `altp`
       thunkobjp `altp`
       blackholeobjp

blackholeobjp :: Parser Token (Maybe Name, Obj)
blackholeobjp = objkindp OBLACKHOLE `usingp` const (namefst BLACKHOLE)

thunkobjp = objkindp OTHUNK `xthenp` cutp "thunkobjp_1"
            (symkindp SymLParen `xthenp`  cutp "thunkobjp_2"
             (exprp  `thenxp` cutp "thunkobjp_3"
              (symkindp SymRParen)))
            `usingp` (namefst . THUNK)

conobjp = (objkindp OCON `xthenp` 
          (symkindp SymLParen`xthenp` 
           (conp `thenp` (manyp atomp `thenxp` symkindp SymRParen))))
         `usingp` (namefst . uncurry CON)

funobjp = (objkindp OFUN `xthenp` cutp "funobjp_1"
          (symkindp SymLParen `xthenp` cutp "funobjp_2"
           (somep varp `thenp` cutp "funobjp_3"
            (symkindp SymArrow `xthenp` cutp "funobjp_4"
             (exprp `thenxp` cutp "funobjp_5"
              (symkindp SymRParen))))))
         `usingp` (namefst . \(vs,e) -> FUN (length vs) vs e)

papobjp = (objkindp OPAP `xthenp` 
          (symkindp SymLParen `xthenp` 
           (varp `thenp` 
            (somep atomp `thenxp` 
             symkindp SymRParen))))
         `usingp` (namefst . uncurry PAP)

exprp =        eprimopp   -- preceed efcallp, primops are just distinguished vars
        `altp` efcallp
        `altp` eletp 
        `altp` ecasep
        `altp` eatomp

eatomp = atomp `usingp` EAtom

efcallp = (varp `thenp` somep atomp) 
          `usingp` \(f,as) -> EFCall f Nothing as

eprimopp = (primopp `thenp` somep atomp) `usingp` uncurry EPrimop

eletp = (kwkindp KWlet `xthenp` cutp "eletp_1"
         (symkindp SymLBrace `xthenp`  cutp "eletp_2"
          (defsp `thenp`  cutp "eletp_3"       -- [def]
           (symkindp SymRBrace `xthenp`  cutp "eletp_4"
            (kwkindp KWin `xthenp`  cutp "eletp_5"
             exprp)))))         -- expr
        `usingp` \(defs, e) -> ELet defs (namefst e)
          
ecasep = (kwkindp KWcase `xthenp` cutp "ecasep_1"                -- case
          (exprp `thenp` cutp "ecasep_2"                          -- expr
           (kwkindp KWof `xthenp`  cutp "ecasep_3"                 -- of
            (symkindp SymLBrace `xthenp`  cutp "ecasep_4"         -- 
             (sepbyp alternp (symkindp SymSemi) `thenxp` cutp "ecasep_5"
              (symkindp SymRBrace)))))) 
         `usingp` \(e,alts) -> ECase (namefst e) $ zip (repeat Nothing) alts
            
alternp = aconp `altp` adefp

aconp = conp `thenp` 
        (manyp varp `thenp` 
         (symkindp SymArrow `xthenp` 
          exprp)) 
        `usingp` \(c,(vs,e)) -> ACon c vs e

adefp = (varp `thenp`
         (symkindp SymArrow `xthenp`
          exprp))
        `usingp` uncurry ADef

