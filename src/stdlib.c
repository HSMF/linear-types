#include <stdint.h>
#include <stdio.h>
#define INT int64_t

void print_int(INT x) { printf("%ld", x); }
void print_ch(INT x) { printf("%c", (char)(x & 0xff)); }
