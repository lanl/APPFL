module Boilerplate (
  header,
  footer
) where


header :: String
header = "#include <stdio.h>\n" ++
         "#include <assert.h>\n" ++
         "#include \"stg.h\"\n" ++
         "#include \"stgutils.h\"\n" ++
         "#include \"cmm.h\"\n" ++
         "#include \"stgcmm.h\"\n" ++
         "#include \"gc.h\"\n" ++
         "\n"
        
footer :: String
footer = "\nDEFUN0(start) {\n" ++
         "  stgPushCont(showResultCont);\n" ++
         "  STGEVAL(((PtrOrLiteral){.argType = HEAPOBJ, .op = &sho_main}));\n" ++
         "  STGRETURN0();\n" ++
         "  ENDFUN;\n" ++
         "}\n\n" ++
         "int main (int argc, char **argv) {\n" ++
         "  initStg();\n" ++
         "  initCmm();\n" ++
         "  initGc();\n" ++
         "  CALL0_0(start);\n" ++
         "  return 0;\n" ++
         "}\n\n"
         