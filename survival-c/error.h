#ifndef ERROR_H
#define ERROR_H

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

char *format(const char *fmt, ...);
void error(char *msg);

#endif
