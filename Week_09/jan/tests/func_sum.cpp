#include <string.h>
#include <gtest/gtest.h>

#include "../include/func_monom.h"
#include "../include/func_x.h"
#include "../include/func_sum.h"
#include "../include/func_constant.h"

TEST(Func_Sum, Sum_of_Constants) {
  Function::Constant_ptr c1_ptr(new Function::Constant("5"));
  Function::Constant_ptr c2_ptr(new Function::Constant("10"));
  Function::Sum s(std::move(c1_ptr), std::move(c2_ptr));

  ASSERT_EQ(15, s(1));
  ASSERT_EQ(15, s(0));
  ASSERT_EQ(15, s(100));

  ASSERT_EQ("0", s.derive());

  ASSERT_EQ("5+10", s.print());
}
