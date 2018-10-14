#include <stdio.h>
#include <stdlib.h>
#include "reciprocal.hpp"

int main (int argc, char **argv)
{
  int i;

  i = atoi (argv[1]);// atoi 문자열을 정수로 바꿈.
  printf ("The reciprocal of %d is %g\n", i, reciprocal (i));
  return 0;
}
