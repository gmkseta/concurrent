#include <stdlib.h>

int main ()
{
  int return_value;
  return_value = system ("ps -o pid,ppid,command");
  return return_value;
}
