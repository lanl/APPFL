module RenameTest where
import Parser
import Rename


import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

unitTests = testGroup "Rename Unit tests"
    [ testCase "rename seq" $ let
    ins = "seq = FUN(x y -> case x of { z -> y });"
    outs = "[FUN {omd = (), vs = [\"x\",\"y\"], \
    \e = ECase {emd = (), ee = EAtom {emd = (), ea = Var \"x\"}, \
    \ealts = Alts {altsmd = (), alts = [ADef {amd = (), av = \"z\", \
    \ae = EAtom {emd = (), ea = Var \"y\"}}], aname = \"alts_0\"}}, \
    \oname = \"seq\"}]"
    in (show $ renameObjs $ parser ins) @?= outs 
    ]