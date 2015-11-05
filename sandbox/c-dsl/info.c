#include "stg.h"

InfoTab it_false  __attribute__((aligned(8))) =
  { .name                = "false",
    // fvs []
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 0,
    // argPerm = []
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "",
    .conFields.arity     = 0,
    .conFields.tag       = 0,
    .conFields.conName   = "False", 
  };

