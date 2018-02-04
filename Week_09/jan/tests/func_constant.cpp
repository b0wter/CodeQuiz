#include <string.h>
#include <gtest/gtest.h>

#include "../include/func_constant.h"

TEST(Func_Constant, Integer) {
  Function::Constant c("5");

  ASSERT_EQ(5, c(0));
  ASSERT_EQ(5, c(1));
  ASSERT_EQ(5, c(100));

  ASSERT_EQ("0", c.derive());

  ASSERT_EQ("5", c.print());
}

TEST(Func_Constant, Double) {
  Function::Constant c("3.14");

  ASSERT_EQ(3.14, c(0));
  ASSERT_EQ(3.14, c(1));
  ASSERT_EQ(3.14, c(100));

  ASSERT_EQ("0", c.derive());

  ASSERT_EQ("3.14", c.print());
}

TEST(Func_Constant, NegativeDouble) {
  Function::Constant c("-42.001");

  ASSERT_EQ(-42.001, c(0));
  ASSERT_EQ(-42.001, c(1));
  ASSERT_EQ(-42.001, c(100));

  ASSERT_EQ("0", c.derive());

  ASSERT_EQ("-42.001", c.print());
}