{-# LANGUAGE CPP #-}

#include "../options.h"

module Options
(
    reWriteSTG,
    useArgType,
    useObjType,
    useInfoTabHeader
) where

#if USE_ARGTYPE
useArgType = True
#else
useArgType = False
#endif

#if USE_OBJTYPE
useObjType = True
#else
useObjType = False
#endif

#if DEBUG_INFOTAB
useInfoTabHeader = True
#else
useInfoTabHeader = False
#endif

-- Flag used to indicate grammar of Case expressions for Parser.hs
-- if True, the scrutinee binding will hold a dummy string
-- this is useful for converting old syntax to new using ConvertSTG.hs
#if REWRITE_STG
reWriteSTG = True
#else
reWriteSTG = False
#endif
