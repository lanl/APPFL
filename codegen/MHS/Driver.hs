module Driver (
  tokenizer,
  parser,
  renamer,
  defaultcaser,
  freevarer,
  infotaber,
  conmaper,
  typechecker,
  knowncaller,
  heapchecker,
  tctest,
  tester,
  printObjsVerbose,
  codegener
) where

import qualified MHS.AST
import qualified MHS.Parser
import qualified MHS.Transform
import qualified MHS.Tokenizer          
import qualified MHS.ToSTG

import           Rename
import           ADT
import           AST
import           SCC
import           CodeGen
import           CMap
import           Analysis
import           PPrint
import           InfoTab
import           HeapObj
import           SetFVs
import           BU
import           HMStg
import           Data.List
import           System.IO

header :: String
header = "#include \"stgc.h\"\n"
        
footer :: Bool -> String
footer v = 
  let top = "\nDEFUN0(start) {\n" ++
            "  registerSHOs();\n" ++
            "  stgPushCont(showResultCont);\n" ++
            "  STGEVAL(((PtrOrLiteral){.argType = HEAPOBJ, .op = &sho_main}));\n" ++
            "  STGRETURN0();\n" ++
            "  ENDFUN;\n" ++
            "}\n\n" ++
            "int main (int argc, char **argv) {\n" ++
            "  initStg();\n" ++
            "  initCmm();\n" ++
            "  initGc();\n" ++
            "  CALL0_0(start);\n"
      bot = "  return 0;\n" ++ "}\n\n"
  in if v then top ++ "  showStgHeap();\n" ++ bot else top ++ bot
           

-- nameDefs
--  :: [([Char], Obj)] ->
--     [([Char], [Char])] ->
--     State [[Char]] [([Char], Obj)]


-- need a better way, like reading from a .h file
stgRTSGlobals :: [String]
stgRTSGlobals = [ "stg_case_not_exhaustive",
                  "true",  -- sho_True
                  "false",  -- sho_False

                  "True#",  
                  "False#" 
                ] ++ map fst primopTab -- from AST.hs


-- strip comments, tokenize input
-- includes basic checking for valid characters and character strings
tokenizer :: String -> [Token]
tokenizer = tokenize

testTokenizer = tester tokenizer (show.pprint)

-- parse tokenized input
-- checks for valid syntax
parser :: String -> [Defn]
parser inp = let toks = tokenizer inp
             in intCon : dblCon : parse toks ---------- BUILD IN BOXED DATATYPES!!!

testparser = tester parser (show.unparse)

-- punp ~= parse . unparse . parse
punp :: String -> [Defn]
punp inp = let defs = parser inp
               out = show $ unparse defs
           in parser out

testpunp = tester punp (show . unparse)


-- Associate type signatures with objects
typeSigSetter :: String -> [Defn]
typeSigSetter inp = let defs = parser inp
                    in setTypeSignatures defs

testSigSetter = tester typeSigSetter (show.pprint)


-- Rename all user objects uniquely
renamer :: String -> [Defn]
renamer inp = let defs = typeSigSetter inp
               in renameIDs defs

testRenamer = tester renamer (show.unparse)


-- Generate functions in place of Constructors and Primops
genFunctioner :: String -> [Defn]
genFunctioner inp = let defs = renamer inp
                       in genFunctions defs

testGenFunctioner = tester genFunctioner (show.unparse)
                    

-- simplify expression application with let bindings
-- All application must be atomic after this
desugarApser :: String -> [Defn]
desugarApser inp = let defs = genFunctioner inp
                   in desugarExpAps defs

testDesugarApser = tester desugarApser (show.unparse)                      


-- Apply built-in Int constructor to all literal Ints in
-- expressions and patterns
numBoxer :: String -> [Defn]
numBoxer inp = let defs = desugarApser inp
               in boxNumLiterals defs

testNumBoxer = tester numBoxer (show.unparse)                  


-- Make all pattern matching in functions and case expressions
-- simple (non-nested) case expressions.
patternSimplifier :: String -> [Defn]
patternSimplifier inp = let defs = numBoxer inp
                        in desugarPatterns defs

testPatSimpler = tester patternSimplifier (show.unparse)


stger inp = let defs = patternSimplifier inp
            in makeSTG defs

testSTGer = tester stger (\(ts, os, as) -> show $ unparse os)


renamer' :: String -> ([TyCon], [Obj ()])
renamer' inp = let (tycons, objs) = stger inp
              in (tycons, renameObjs objs, assums)


-- generate default cases in Alts blocks that need them
-- The ordering here is important to allow the freevarer and
-- infotaber steps to properly handle the newly generated ADef
-- objects
-- might be worth starting to pass CMap around starting here
-- (currently generated again when setting CMaps in InfoTabs)
defaultcaser :: String -> ([TyCon], [Obj ()])
defaultcaser inp = let (tycons, objs) = renamer' inp
                 in (tycons, exhaustCases (toCMap tycons) objs, assums)

freevarer :: String -> ([TyCon], [Obj ([Var],[Var])], Assumptions)
freevarer inp = let (tycons, objs, assums) = defaultcaser inp
                in (tycons, setFVsObjs stgRTSGlobals objs, assums)

infotaber :: String -> ([TyCon], [Obj InfoTab], Assumptions)
infotaber inp = let (tycons, objs, assums) = freevarer inp
                in (tycons, setITs objs :: [Obj InfoTab], assums)

conmaper :: String -> ([TyCon], [Obj InfoTab], Assumptions)
conmaper inp = let (tycons, objs, assums) = infotaber inp
                   (tys, os) = setCMaps tycons objs
               in (tys, os, assums)

typechecker :: String -> ([TyCon], [Obj InfoTab])
typechecker inp = let (tycons, objs, assums) = conmaper inp
                  in (tycons, hmstg objs assums)


knowncaller inp  = let (tycons, objs) = typechecker inp
                   in (tycons, propKnownCalls objs)

heapchecker inp = let (tycons, objs) = knowncaller inp
                  in (tycons, setHeapAllocs objs)

               
printObjsVerbose (tycons, objs) = print $ objListDoc objs



tester tfun sfun infile =
 do
   ihandle <- openFile infile ReadMode
   _tester tfun sfun ihandle stdout
  
_tester testfun showfun ifd ofd =
  do
    inp <- hGetContents ifd
    let res = testfun inp
        out = showfun res
    hPutStrLn ofd out
    return ()


tctest arg =
  do
    ifd <- openFile arg ReadMode
    source <- hGetContents ifd
    let (tycons, objs, assums) = conmaper source
    hmstgdebug objs assums


codegener :: String -> Bool -> Bool -> String
codegener inp v mhs = let (tycons, objs) = if mhs then undefined
                                           else heapchecker inp
                      infotab = showITs objs
                      (shoForward, shoDef) = showSHOs objs
                      (funForwards, funDefs) = cgObjs objs stgRTSGlobals
                 in header ++
                    intercalate "\n" funForwards ++ "\n" ++
                    infotab ++ "\n" ++
                    shoForward ++ "\n" ++
                    shoDef ++ "\n" ++
                    intercalate "\n\n" funDefs ++
                    footer v

