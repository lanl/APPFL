module ParserTest where
import Parser


import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

unitTests = testGroup "Parser Unit tests"
    [ testCase "parser 1" $ let 
    ins = "one=CON(I 1); main=THUNK(one);" 
    outs = "[CON {omd = (), c = \"I\", as = [Lit 1], oname = \"one\"},\
    \THUNK {omd = (), e = EAtom {emd = (), ea = Var \"one\"}, \
    \oname = \"main\"}]"
    in (show $ parser ins) @?= outs 
     
    , testCase "parser seq" $ let
    ins = "seq = FUN(x y -> case x of { z -> y });"
    outs = "[FUN {omd = (), vs = [\"x\",\"y\"], \
    \e = ECase {emd = (), ee = EAtom {emd = (), ea = Var \"x\"}, \
    \ealts = Alts {altsmd = (), alts = [ADef {amd = (), av = \"z\", \
    \ae = EAtom {emd = (), ea = Var \"y\"}}], aname = \"alts\"}}, \
    \oname = \"seq\"}]"
    in (show $ parser ins) @?= outs    
    ]