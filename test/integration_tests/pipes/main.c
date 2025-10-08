#include <stdio.h>

int main() {
  fprintf(stdout, "OUT\n");
  fprintf(stderr, "ERR\n");
  fprintf(stdout, "END\n");

  return 0;
}
