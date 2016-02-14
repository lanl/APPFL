#ifndef log_h
#define log_h

#include "options.h"
#include <stdio.h>
#include <stdarg.h>

extern int LoggingLevel;

typedef enum {
  LOG_NONE = 0,
  LOG_FATAL,
  LOG_ERROR,
  LOG_RESULT,
  LOG_WARNING,
  LOG_INFO,
  LOG_DEBUG,
  LOG_SPEW,
} LogLevel;

static inline __attribute__((always_inline)) 
void log(LogLevel priority, const char *format, ...)
{
  if(LOG_LEVEL >= priority) { // compile time
    if(LoggingLevel >= priority) { // runtime
      va_list args;
      va_start(args, format);
      vfprintf(stderr,format, args);
      va_end(args);
    }
  }
}

#endif
