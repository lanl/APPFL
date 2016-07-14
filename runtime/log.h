#ifndef log_h
#define log_h

#include "options.h"
#include <stdio.h>
#include <stdarg.h>

struct rtArg;

typedef enum {
  LOG_NONE = 0,
  LOG_RESULT,
  LOG_FATAL,
  LOG_ERROR,
  LOG_WARNING,
  LOG_INFO,
  LOG_DEBUG,
  LOG_SPEW,
} LogLevel;

#define LOG(priority, ...) \
do {	\
  if(LOG_LEVEL >= (priority)) {  \
    if(rtArg.loggingLevel >= (priority)) {  \
      fprintf(stderr, __VA_ARGS__); \
    } \
  } \
} while (0)

#endif
