#include <string.h>
#include <gtest/gtest.h>

#include "../include/func_x.h"

TEST(Func_X, Integer) {
  Function::X c("x");

  ASSERT_EQ(0, c(0));
  ASSERT_EQ(1, c(1));
  ASSERT_EQ(100, c(100));

  ASSERT_EQ("1", c.derive());

  ASSERT_EQ("x", c.print());

  ASSERT_THROW(Function::X c("1"), std::runtime_error);
}
