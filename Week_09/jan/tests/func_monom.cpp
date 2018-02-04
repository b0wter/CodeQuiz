#include <string.h>
#include <gtest/gtest.h>

#include "../include/func_monom.h"
#include "../include/func_x.h"
#include "../include/func_constant.h"

TEST(Func_Monom, Monom_of_Constant) {
  Function::Constant_ptr c_ptr(new Function::Constant("5"));
  Function::Monom m("5^2", std::move(c_ptr));

  ASSERT_EQ(25, m(0));
  ASSERT_EQ(25, m(1));
  ASSERT_EQ(25, m(100));

  ASSERT_EQ("0", m.derive());

  ASSERT_EQ("5^2", m.print());

  ASSERT_THROW(Function::Monom m("5^2", nullptr), std::runtime_error);
}

TEST(Func_Monom, Monom_of_Constant_with_wrong_definition) {
  Function::Constant_ptr c_ptr(new Function::Constant("5"));
  ASSERT_THROW(Function::Monom m("1", std::move(c_ptr)), std::runtime_error);
}

TEST(Func_Monom, Monom_of_X) {
  Function::X_ptr x_ptr(new Function::X("x"));
  Function::Monom m("x^2", std::move(x_ptr));

  ASSERT_EQ(0, m(0));
  ASSERT_EQ(1, m(1));
  ASSERT_EQ(25, m(5));

  ASSERT_EQ("2x", m.derive());

  ASSERT_EQ("x^2", m.print());
}

TEST(Func_Monom, Monom_of_X_high_order) {
  Function::X_ptr x_ptr(new Function::X("x"));
  Function::Monom m("x^8", std::move(x_ptr));

  ASSERT_EQ(0, m(0));
  ASSERT_EQ(1, m(1));
  ASSERT_EQ(390625, m(5));

  ASSERT_EQ("8x^7", m.derive());

  ASSERT_EQ("x^8", m.print());
}

TEST(Func_Monom, Monom_of_X_sqrt) {
  Function::X_ptr x_ptr(new Function::X("x"));
  Function::Monom m("x^-0.5", std::move(x_ptr));

  ASSERT_EQ(std::numeric_limits<double>::infinity(), m(0));
  ASSERT_EQ(1, m(1));
  ASSERT_EQ(0.25, m(16));
  ASSERT_EQ(2, m(0.25));

  ASSERT_EQ("-0.5x^-1.5", m.derive());

  ASSERT_EQ("x^-0.5", m.print());
}