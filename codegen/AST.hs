module AST (
  Var,
  Con,
  Atom(..),
  Expr(..),
  Alt(..),
  Alts(..),
  Obj(..),
  Primop(..),
  primopTab,
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

data Atom = Var  Var
          | LitI Int
          | LitB Bool
          | LitF Float
          | LitD Double
          | LitC Char
            deriving(Eq,Show)

data Expr a = EAtom   {emd :: a,                    ea :: Atom} --,      ename::String}
            | EFCall  {emd :: a, ev :: Var,         eas :: [Atom]} --,   ename::String}
            | EPrimop {emd :: a, eprimop :: Primop, eas :: [Atom]} --,   ename::String}
            | ELet    {emd :: a, edefs :: [Obj a],  ee :: Expr a} --,    ename::String}
            | ECase   {emd :: a, ee :: Expr a,      ealts :: Alts a} --, ename::String}
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

-- when calculating free variables we need an enclosing environment that
-- includes the known primops.  This will allow proper scoping.  As such
-- we initially parse EPrimops as EFCalls, then transform and check saturation
-- here.  We only need to distinguish EPrimop from EFCall because special
-- code is generated for them.

data Primop = Piadd -- Int -> Int -> Int
            | Pisub 
            | Pimul
            | Pidiv
            | Pimod
            | Pimax
            | Pimin

            | Pieq -- Int -> Int -> Bool
            | Pine
            | Pilt
            | Pile
            | Pigt
            | Pige

            | Pineg -- Int -> Int

            -- the following are deprecated
            | PintToBool
              deriving(Eq,Show)

-- these are the C names, not STG names
primopTab = 
    [("iplus_h",  Piadd), 
     ("isub_h",   Pisub),
     ("imul_h",   Pimul),
     ("idiv_h",   Pidiv),
     ("imod_h",   Pimod),
    
     ("ieq_h",    Pieq),
     ("ine_h",    Pine),
     ("ilt_h",    Pilt),
     ("ile_h",    Pile),
     ("igt_h",    Pigt),
     ("ige_h",    Pige),

     ("ineg_h",   Pineg),

     ("imin_h",   Pimax),
     ("imax_h",   Pimin),

     -- the following are deprecated
     ("intToBool_h", PintToBool)
    ]
