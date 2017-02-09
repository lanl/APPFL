#ifndef log_h
#define log_h

#include "options.h"
#include <stdio.h>
#include <stdarg.h>
#include "abt.h"

struct rtArg;

extern ABT_mutex loglock;

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
      ABT_mutex_lock(loglock); \
      fprintf(stderr, __VA_ARGS__); \
      ABT_mutex_unlock(loglock); \
    } \
  } \
} while (0)

#endif
