#ifndef CRUNCH_AVR_ENVIRONMENT_H
#define CRUNCH_AVR_ENVIRONMENT_H

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define PRIu64 "lu"
#define PRIi64 "ld"
#define assert(...)                                                            \
  do {                                                                         \
  } while (0)

static int errno;
static FILE *fmemopen(...) { return 0; }
static void crunch_init_host(void) {}

#endif // CRUNCH_AVR_ENVIRONMENT_H
