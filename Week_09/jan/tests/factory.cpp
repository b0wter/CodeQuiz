#include <string.h>
#include <gtest/gtest.h>

#include "../include/factory.h"

TEST(Func_Factory, Constant) {
  auto fn = Function::Factory::parse("2");

  ASSERT_EQ(2, fn->eval(-2));
  ASSERT_EQ(2, fn->eval(2));
  ASSERT_EQ("2", fn->print());
  ASSERT_EQ("0", fn->derive());
}

TEST(Func_Factory, Sum_of_Constant) {
  auto fn = Function::Factory::parse("2+8");

  ASSERT_EQ(10, fn->eval(2));
  ASSERT_EQ(10, fn->eval(-2));
  ASSERT_EQ("2+8", fn->print());
  ASSERT_EQ("0", fn->derive());
}

TEST(Func_Factory, X) {
  auto fn = Function::Factory::parse("x");

  ASSERT_EQ(-2, fn->eval(-2));
  ASSERT_EQ(2, fn->eval(2));
  ASSERT_EQ("x", fn->print());
  ASSERT_EQ("1", fn->derive());
}

TEST(Func_Factory, Klammer) {
  auto fn = Function::Factory::parse("(2))");

  ASSERT_EQ(2, fn->eval(-2));
  ASSERT_EQ(2, fn->eval(2));
  ASSERT_EQ("2", fn->print());
  ASSERT_EQ("0", fn->derive());
}

TEST(Func_Factory, 5X) {
  auto fn = Function::Factory::parse("5x");

  ASSERT_EQ(-10, fn->eval(-2));
  ASSERT_EQ(10, fn->eval(2));
  ASSERT_EQ("5x", fn->print());
  ASSERT_EQ("5", fn->derive());
}

TEST(Func_Factory, Sum_of_Constants) {
  auto fn = Function::Factory::parse("2+8+10+2");

  ASSERT_EQ(22, fn->eval(2));
  ASSERT_EQ(22, fn->eval(-2));
  ASSERT_EQ("2+8+10+2", fn->print());
  ASSERT_EQ("0", fn->derive());
}

TEST(Func_Factory, Sum_of_Constant_and_X) {
  auto fn = Function::Factory::parse("2+x");

  ASSERT_EQ(4, fn->eval(2));
  ASSERT_EQ(0, fn->eval(-2));
  ASSERT_EQ("2+x", fn->print());
  ASSERT_EQ("1", fn->derive());
}

TEST(Func_Factory, Sum_of_Constant_and_2X) {
  auto fn = Function::Factory::parse("2+3x");

  ASSERT_EQ(8, fn->eval(2));
  ASSERT_EQ(-4, fn->eval(-2));
  ASSERT_EQ("2+3x", fn->print());
  ASSERT_EQ("3", fn->derive());
}