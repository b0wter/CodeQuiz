#include <string.h>
#include <gtest/gtest.h>

#include "../include/func_constant.h"

TEST(Func_Constant, Integer) {
  Function::Constant c("5");

  ASSERT_EQ(5, c.eval(0));
  ASSERT_EQ(5, c.eval(1));
  ASSERT_EQ(5, c.eval(100));

  ASSERT_EQ("0", c.derive());

  ASSERT_EQ("5", c.print());
}

TEST(Func_Constant, Double) {
  Function::Constant c("3.14");

  ASSERT_EQ(3.14, c.eval(0));
  ASSERT_EQ(3.14, c.eval(1));
  ASSERT_EQ(3.14, c.eval(100));

  ASSERT_EQ("0", c.derive());

  ASSERT_EQ("3.14", c.print());
}

TEST(Func_Constant, NegativeDouble) {
  Function::Constant c("-42.001");

  ASSERT_EQ(-42.001, c.eval(0));
  ASSERT_EQ(-42.001, c.eval(1));
  ASSERT_EQ(-42.001, c.eval(100));

  ASSERT_EQ("0", c.derive());

  ASSERT_EQ("-42.001", c.print());
}