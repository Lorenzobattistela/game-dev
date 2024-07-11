#include "error.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

char *format(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  int len = vsnprintf(NULL, 0, fmt, args);
  va_end(args);
  char *buf = malloc(len+1);
  va_start(args, fmt);
  vsnprintf(buf, len+1, fmt, args); 
  va_end(args);
  return buf;
}

void error(char *msg) {
  fprintf(stderr, "ERROR: %s\n", msg);
  exit(1);
}
