module Boilerplate (
  header,
  footer
) where


header :: String
header = "#include \"stg_header.h\"\n"
        
footer :: String
footer = "\nDEFUN0(start) {\n" ++
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
         "  CALL0_0(start);\n" ++
         "  showStgHeap();\n" ++
         "  return 0;\n" ++
         "}\n\n"
         