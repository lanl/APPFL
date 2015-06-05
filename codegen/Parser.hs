{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}

module Parser (
  parse,
  showObjs,
) where

import AST
import Lexer
import ParseComb
import ADT
import Data.List

{-  grammar

<var> :: C syntax, more or less

<con> :: start with uppercase

<lit> ::= int[eger] | "true#" | "false#"

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
                        
-- type Token = (Tag, [Char])
-- type Parser a b = [a] -> [(b, [a])]

--- layer for adding ADT defs

parse :: [Char] ->  ([TyCon], [Obj ()])
parse inp = case defdatsp $ lexer inp of
               [] ->  error "parser failed"
               xs -> if snd (head xs) /= [] 
                     then error ("leftover input on parse: " ++ show (snd $ head xs))
                     else splitDefs $ fst $ head xs

splitDefs :: [Def a] -> ([TyCon], [Obj a])
splitDefs d = (getDatas d, getObjs d)
                   
getObjs :: [Def a] -> [Obj a]
getObjs = concatMap getObj
          where getObj (ObjDef o) = [o]
                getObj (DataDef _) = []

getDatas :: [Def a] -> [TyCon]
getDatas = concatMap getData
           where getData (ObjDef _) = []
                 getData (DataDef t) = [t]

defdatsp :: Parser Token [Def ()]
defdatsp = sepbyp defdatp (symkindp SymSemi) `thenxp` optlp (symkindp SymSemi)

defdatp :: Parser Token (Def ())
defdatp = (defp `usingp` ObjDef) `altp` (tyconp `usingp` DataDef)

tyconp :: Parser Token TyCon
tyconp = (kwkindp KWdata `xthenp` kwkindp KWunboxed `xthenp` tyconp' False)
         `altp` (kwkindp KWdata `xthenp` tyconp' True)

tyconp' :: Bool -> Parser Token TyCon  
tyconp' boxed = (conp `thenp`
                 ((manyp varp) `thenp`
                  (symkindp SymBind `xthenp`
                   (sepbyp (dataconp boxed) (symkindp SymPipe)))))
                 `usingp` \(t,(vs,defs)) -> TyCon boxed t vs defs
 
dataconp :: Bool -> Parser Token DataCon
dataconp boxed = (conp `thenp` manyp (monop boxed))
                 `usingp` uncurry (DataCon boxed)

monop :: Bool -> Parser Token Monotype
monop boxed = ((symkindp SymLParen `xthenp` (monop' boxed)
              `thenxp` symkindp SymRParen) `altp`
              (monop' boxed)) 

monop' :: Bool -> Parser Token Monotype
monop' boxed = (varp `usingp` MVar) `altp`
              ((conp `thenp` manyp (monop boxed)) `usingp` uncurry (MCon undefined)) `altp`
               -- require parens on function type or we get into a loop
               ((symkindp SymLParen `xthenp` monop boxed `thenxp`
               symkindp SymArrow `thenp` monop boxed `thenxp` symkindp SymRParen)
               `usingp` uncurry MFun)

--- end layer for adding ADT defs

symkindp :: Symbol -> [Token] -> [(Symbol, [Token])]
symkindp s1 ((Sym s2):xs) | s1 == s2 = succeedp s1 xs
symkindp _ _ = failp []

objkindp :: Object -> [Token] -> [(Object, [Token])]
objkindp o1 ((Obj o2):xs) | o1 == o2 = succeedp o1 xs
objkindp _ _ = failp []

kwkindp :: Keyword -> [Token] -> [(Keyword, [Token])]
kwkindp k1 ((KW k2):xs) | k1 == k2 = succeedp k1 xs
kwkindp _ _ = failp []

bikindp :: BuiltinType -> [Token] -> [(BuiltinType, [Token])]
bikindp b1 ((BIT b2):xs) | b1 == b2 = succeedp b1 xs
bikindp _ _ = failp []

nump :: Parser Token Int
nump ((BIInt i) : xs) = succeedp i xs
nump _ = failp []

boolp :: Parser Token Bool
boolp (BIBool i : xs) = succeedp i xs
boolp _ = failp []

varp :: Parser Token String
varp ((Ident s) : xs) = succeedp s xs
varp _ = failp []

conp :: Parser Token String
conp ((Ctor c) : xs) = succeedp c xs
conp _ = failp []

atomp :: Parser Token Atom
atomp = (nump `usingp` LitI) `altp` 
        (varp `usingp` Var)  `altp`
        (boolp `usingp` LitB)

defsp :: Parser Token [Obj ()]
defsp = sepbyp defp (symkindp SymSemi) `thenxp` optlp (symkindp SymSemi)

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

thunkobjp :: Parser Token (Obj ())
thunkobjp = objkindp OTHUNK `xthenp` cutp "thunkobjp_1"
            (symkindp SymLParen `xthenp`  cutp "thunkobjp_2"
             (exprp  `thenxp` cutp "thunkobjp_3"
              (symkindp SymRParen)))
            `usingp` \e ->  THUNK () e ""

conobjp :: Parser Token (Obj ())
conobjp = (objkindp OCON `xthenp` 
          (symkindp SymLParen`xthenp` 
           (conp `thenp` (manyp atomp `thenxp` symkindp SymRParen))))
         `usingp` \(c,as) -> CON () c as ""

funobjp :: Parser Token (Obj ())
funobjp = (objkindp OFUN `xthenp` cutp "funobjp_1"
          (symkindp SymLParen `xthenp` cutp "funobjp_2"
           (somep varp `thenp` cutp "funobjp_3"
            (symkindp SymArrow `xthenp` cutp "funobjp_4"
             (exprp `thenxp` cutp "funobjp_5"
              (symkindp SymRParen))))))
         `usingp` \(f,as) -> FUN () f as ""

papobjp :: Parser Token (Obj ())
papobjp = (objkindp OPAP `xthenp` 
          (symkindp SymLParen `xthenp` 
           (varp `thenp` 
            (somep atomp `thenxp` 
             symkindp SymRParen))))
         `usingp` \(p,as) -> (PAP () p as "")

exprp :: Parser Token (Expr ())
exprp =        eprimopp   -- precede efcallp, primops are just distinguished vars
        `altp` efcallp
        `altp` eletp 
        `altp` ecasep
        `altp` eatomp

eatomp :: Parser Token (Expr ())
eatomp = atomp `usingp` (EAtom ())

efcallp :: Parser Token (Expr ())
efcallp = (varp `thenp` somep atomp) 
          `usingp` uncurry (EFCall ())

primopp :: [Token] -> [(Primop, [Token])]
primopp inp = case varp inp of
                [] -> failp []
                [(v,inp')] -> 
                    case lookup v primopTab of
                      Nothing -> failp []
                      Just p -> [(p,inp')]

-- primopp ((PO po):xs) = succeedp po xs
-- primopp _ = failp []

eprimopp :: Parser Token (Expr ())
eprimopp = (primopp `thenp` somep atomp) `usingp` uncurry (EPrimop ())

eletp :: Parser Token (Expr ())
eletp = (kwkindp KWlet `xthenp` cutp "eletp_1"
         (symkindp SymLBrace `xthenp`  cutp "eletp_2"
          (defsp `thenp`  cutp "eletp_3"       -- [def]
           (symkindp SymRBrace `xthenp`  cutp "eletp_4"
            (kwkindp KWin `xthenp`  cutp "eletp_5"
             exprp)))))         -- expr
        `usingp` uncurry (ELet ())
          
ecasep :: Parser Token (Expr ())
ecasep = (kwkindp KWcase `xthenp` cutp "ecasep_1"                -- case
          (exprp `thenp` cutp "ecasep_2"                          -- expr
           (kwkindp KWof `xthenp`  cutp "ecasep_3"                 -- of
            (symkindp SymLBrace `xthenp`  cutp "ecasep_4"         -- 
             (alternsp `thenxp` cutp "ecasep_5:  } expected instead of "
              (symkindp SymRBrace)))))) 
         `usingp` uncurry (ECase ())

alternsp :: Parser Token (Alts ())
alternsp = sepbyp alternp (symkindp SymSemi) 
           `usingp` \alts -> Alts () alts "alts"
            
alternp :: Parser Token (Alt ())
alternp = aconp `altp` adefp

aconp :: Parser Token (Alt ())
aconp = conp `thenp` 
        (manyp varp `thenp` 
         (symkindp SymArrow `xthenp` 
          exprp)) 
        `usingp` \(c,(vs,e)) -> ACon () c vs e

adefp :: Parser Token (Alt ())
adefp = (varp `thenp`
         (symkindp SymArrow `xthenp`
          exprp))
        `usingp` uncurry (ADef ())


-- unparser ****************************************************************
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

showObjs objs = interpolate $ intercalate "\n" $ unparser 0 objs

indent n s@('%':_) = s
indent n s = (take n $ repeat ' ') ++ s

indents n ss = map (indent n) ss

-- instance Unparser n Atom where
showa (Var v) = v
showa (LitI i) = show i
showa (LitB False) = "False#"
showa (LitB True) = "True#"


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

