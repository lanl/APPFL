#include "args.h"
#include "options.h"
#include <getopt.h>
#include <stdlib.h>

float GCThreshold = 0.0;

int evalStrategy = LAZY;

int constStrict = 0;

bool sanityChecker = false;

int perfCounters = 0;

int LoggingLevel = LOG_LEVEL;


void parseArgs (int argc, char **argv) {
  int c;
  while ((c = getopt (argc, argv, "gt:e:c:l:sp:")) != -1)
    switch (c) {
    case 'g':
      GCThreshold = 1.0; // never run
      break;

    case 't':
      GCThreshold = atof(optarg);
      break;

    case 'e':
      evalStrategy = atoi(optarg);
      break;
      
    case 'c':
      constStrict = atoi(optarg);
      break;

    case 'l':
      LoggingLevel = atoi(optarg);
      break;
      
    case 's':
      sanityChecker = true;
      break;
      
    case 'p':
      perfCounters = atoi(optarg);
      break;
        
    }
}
