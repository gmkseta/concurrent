#include <cassert>
#include "reciprocal.hpp"

double reciprocal (int i) {
  // 0이 아니면 종료됨
  assert (i != 0);
  return 1.0/i;
}
