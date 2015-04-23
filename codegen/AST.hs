module AST (
  Var,
  Con,
  Atom(..),
  Expr(..),
  Alt(..),
  Alts(..),
  Obj(..),
  Primop(..),
) where

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
             
data Primop = Piadd 
            | Pisub 
            | Pimul
            | Pidiv
            | Pimod

            | Pieq
            | Pine
            | Pilt
            | Pile
            | Pigt
            | Pige

            | Pineg

            | Pimax
            | Pimin

            | PintToBool
              deriving(Eq,Show)
              
       
             
