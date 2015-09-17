#include "args.h"
#include <getopt.h>
#include <stdlib.h>

float GCThreshold = 0.0;

void parseArgs (int argc, char **argv) {
  int c;
  while ((c = getopt (argc, argv, "gt:")) != -1)
    switch (c)
    {
    case 'g':
      GCThreshold = 1.0; // never run
      break;

    case 't':
      GCThreshold = atof(optarg);
      break;

    }
}
