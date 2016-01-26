#include "args.h"
#include <getopt.h>
#include <stdlib.h>

float GCThreshold = 0.0;

int evalStrategy = LAZY;

void parseArgs (int argc, char **argv) {
  int c;
  while ((c = getopt (argc, argv, "gt:e:")) != -1)
    switch (c)
    {
    case 'g':
      GCThreshold = 1.0; // never run
      break;

    case 't':
      GCThreshold = atof(optarg);
      break;

    case 'e':
      evalStrategy = atoi(optarg);
      break;
    }
}
