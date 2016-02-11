{-# LANGUAGE CPP #-}

#include "../options.h"

module Options
(
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
