#include <string.h>
#include <gtest/gtest.h>

#include "../include/func_constant.h"


TEST(Func_Constant, Eins) {
  Function::Constant c("");

  ASSERT_EQ(0, c.eval(0));
  ASSERT_EQ("-", c.derive());
}
