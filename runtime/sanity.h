#ifndef SANITY_H
#define SANITY_H

#include "log.h"
#include <ctype.h>

void heapCheck(bool display, LogLevel logLevel);
void stackCheck(bool display, LogLevel logLevel);

#endif
