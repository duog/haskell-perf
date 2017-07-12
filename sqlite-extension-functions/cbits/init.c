#include <stdlib.h>
#include <stdio.h>
#include "HsFFI.h"

static void init() __attribute__((constructor));
static void deinit() __attribute__((destructor));

void init(void){
  int argc = 2;
  char *argv[] = { "+RTS", "-A32m", NULL };
  char **pargv = argv;

  // Initialize Haskell runtime
  hs_init(&argc, &pargv);
  printf("init");
}

void end(void){
  printf("deinit");
  hs_exit();
}
