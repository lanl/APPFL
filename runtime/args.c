#include "args.h"
#include "options.h"
#include <getopt.h>
#include <stdlib.h>

Args rtArg = { .gcThreshold = 0.0,
               .evalStrategy = LAZY,
               .constStrict = 0,
               .sanityChecker = false,
               .perfCounters = 0,
               .loggingLevel = LOG_LEVEL
             };


void parseArgs (int argc, char **argv) {
  int c;
  while ((c = getopt (argc, argv, "gt:e:c:l:sp:")) != -1)
    switch (c) {
    case 'g':
      rtArg.gcThreshold = 1.0; // never run
      break;

    case 't':
      rtArg.gcThreshold = atof(optarg);
      break;

    case 'e':
      rtArg.evalStrategy = atoi(optarg);
      break;

    case 'c':
      rtArg.constStrict = atoi(optarg);
      break;

    case 'l':
      rtArg.loggingLevel = atoi(optarg);
      break;

    case 's':
      rtArg.sanityChecker = true;
      break;

    case 'p':
      rtArg.perfCounters = atoi(optarg);
      break;

    }
}
