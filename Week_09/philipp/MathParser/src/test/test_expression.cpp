#include <gtest/gtest.h>

#include <iostream>

#include "expression.h"

TEST(Expression, NullExpr) {
    std::ostringstream os;
    NullExpr n;
    EXPECT_EQ(n.evaluate(), DBL_MAX);
    EXPECT_EQ(n.derivative(), nullptr);
    EXPECT_EQ(n.type(), NodeTypes::NullExpr);
    n.print(os, 0);
    EXPECT_EQ(os.str(), "");
}

TEST(Expression, ConstantExpr) {
    std::ostringstream os;
    ConstantExpr c(2);
    EXPECT_EQ(c.evaluate(), double(2));
    EXPECT_EQ(c.type(), NodeTypes::ConstantExpr);
    ExprNode *d = c.derivative();
    EXPECT_EQ(d->type(), NodeTypes::NullExpr);
    c.print(os, 0);
    EXPECT_EQ(os.str(), "C(2)");
    delete d;
}

TEST(Expression, ParameterExpr) {
    std::ostringstream os;
    ParameterExpr p("a");
    EXPECT_EQ(p.evaluate(), DBL_MAX);
    EXPECT_EQ(p.type(), NodeTypes::ParameterExpr);
    ExprNode *d = p.derivative();
    EXPECT_EQ(d->type(), NodeTypes::NullExpr);
    p.print(os, 0);
    EXPECT_EQ(os.str(), "P(a)");
    delete d;
}

TEST(Expression, VariableExpr) {
    std::ostringstream os;
    VariableExpr v("x");
    EXPECT_EQ(v.evaluate(), DBL_MAX);
    EXPECT_EQ(v.type(), NodeTypes::VariableExpr);
    ExprNode *d = v.derivative();
    EXPECT_EQ(d->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(d->evaluate(), double(1));
    v.print(os, 0);
    EXPECT_EQ(os.str(), "V(x)");
    delete d;
}

TEST(Expression, NegateExpr) {
    // Single Negate
    std::ostringstream os;
    NegateExpr n(new VariableExpr("x"));
    EXPECT_EQ(n.type(), NodeTypes::NegateExpr);
    EXPECT_EQ(n.innerNode()->type(), NodeTypes::VariableExpr);
    n.print(os, 0);
    EXPECT_EQ(os.str(), "(-)V(x)");
    ExprNode *d = n.derivative();
    EXPECT_EQ(d->type(), NodeTypes::NegateExpr);
    EXPECT_EQ(((NegateExpr*)d)->innerNode()->type(), NodeTypes::ConstantExpr);

    // Double Negate
    // TODO
}

// Constant + Constant
TEST(Expression, AddExpr_Const_Plus_Const) {
    std::ostringstream os;
    AddExpr a(new ConstantExpr(2), new ConstantExpr(3));
    EXPECT_EQ(a.type(), NodeTypes::AddExpr);
    EXPECT_EQ(a.leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(a.rightNode()->type(), NodeTypes::ConstantExpr);
    a.print(os, 0);
    EXPECT_EQ(os.str(), "ADD(C(2), C(3))");
    ExprNode *d = a.derivative();
    ASSERT_EQ(d->type(), NodeTypes::NullExpr);
}

// Constant + !Constant
TEST(Expression, AddExpr_Const_Plus_Var) {
    std::ostringstream os;
    AddExpr a(new ConstantExpr(2), new VariableExpr("x"));
    EXPECT_EQ(a.type(), NodeTypes::AddExpr);
    EXPECT_EQ(a.leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(a.rightNode()->type(), NodeTypes::VariableExpr);
    a.print(os, 0);
    EXPECT_EQ(os.str(), "ADD(C(2), V(x))");
    ExprNode *d = a.derivative();
    ASSERT_EQ(d->type(), NodeTypes::ConstantExpr);
}

// !Constant + Constant
TEST(Expression, AddExpr_Var_Plus_Const) {
    std::ostringstream os;
    AddExpr a(new VariableExpr("x"), new ConstantExpr(2));
    EXPECT_EQ(a.type(), NodeTypes::AddExpr);
    EXPECT_EQ(a.leftNode()->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(a.rightNode()->type(), NodeTypes::ConstantExpr);
    a.print(os, 0);
    EXPECT_EQ(os.str(), "ADD(V(x), C(2))");
    ExprNode *d = a.derivative();
    ASSERT_EQ(d->type(), NodeTypes::ConstantExpr);
}
    
// !Constant + !Constant
TEST(Expression, AddExpr_Var_Plus_Var) {
    std::ostringstream os;
    AddExpr a(new VariableExpr("x"), new VariableExpr("x"));
    EXPECT_EQ(a.type(), NodeTypes::AddExpr);
    EXPECT_EQ(a.leftNode()->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(a.rightNode()->type(), NodeTypes::VariableExpr);
    a.print(os, 0);
    EXPECT_EQ(os.str(), "ADD(V(x), V(x))");
    ExprNode *d = a.derivative();
    ASSERT_EQ(d->type(), NodeTypes::AddExpr);
    ASSERT_EQ(((AddExpr*)d)->leftNode()->type(), NodeTypes::ConstantExpr);
    ASSERT_EQ(((AddExpr*)d)->rightNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(((ConstantExpr*)((AddExpr*)d)->leftNode())->evaluate(), double(1));
    EXPECT_EQ(((ConstantExpr*)((AddExpr*)d)->rightNode())->evaluate(), double(1));
}

// Constant - Constant
TEST(Expression, SubtractExpr_Const_Minus_Const) {
    std::ostringstream os;
    SubtractExpr a(new ConstantExpr(2), new ConstantExpr(3));
    EXPECT_EQ(a.type(), NodeTypes::SubtractExpr);
    EXPECT_EQ(a.leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(a.rightNode()->type(), NodeTypes::ConstantExpr);
    a.print(os, 0);
    EXPECT_EQ(os.str(), "SUB(C(2), C(3))");
    ExprNode *d = a.derivative();
    ASSERT_EQ(d->type(), NodeTypes::NullExpr);
}

// Constant - !Constant
TEST(Expression, SubtractExpr_Const_Minus_Var) {
    std::ostringstream os;
    SubtractExpr a(new ConstantExpr(2), new VariableExpr("x"));
    EXPECT_EQ(a.type(), NodeTypes::SubtractExpr);
    EXPECT_EQ(a.leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(a.rightNode()->type(), NodeTypes::VariableExpr);
    a.print(os, 0);
    EXPECT_EQ(os.str(), "SUB(C(2), V(x))");
    ExprNode *d = a.derivative();
    ASSERT_EQ(d->type(), NodeTypes::ConstantExpr);
}

// !Constant - Constant
TEST(Expression, SubtractExpr_Var_Minus_Const) {
    std::ostringstream os;
    SubtractExpr a(new VariableExpr("x"), new ConstantExpr(2));
    EXPECT_EQ(a.type(), NodeTypes::SubtractExpr);
    EXPECT_EQ(a.leftNode()->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(a.rightNode()->type(), NodeTypes::ConstantExpr);
    a.print(os, 0);
    EXPECT_EQ(os.str(), "SUB(V(x), C(2))");
    ExprNode *d = a.derivative();
    ASSERT_EQ(d->type(), NodeTypes::ConstantExpr);
}
    
// !Constant - !Constant
TEST(Expression, SubtractExpr_Var_Minus_Var) {
    std::ostringstream os;
    SubtractExpr a(new VariableExpr("x"), new VariableExpr("x"));
    EXPECT_EQ(a.type(), NodeTypes::SubtractExpr);
    EXPECT_EQ(a.leftNode()->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(a.rightNode()->type(), NodeTypes::VariableExpr);
    a.print(os, 0);
    EXPECT_EQ(os.str(), "SUB(V(x), V(x))");
    ExprNode *d = a.derivative();
    ASSERT_EQ(d->type(), NodeTypes::SubtractExpr);
    ASSERT_EQ(((AddExpr*)d)->leftNode()->type(), NodeTypes::ConstantExpr);
    ASSERT_EQ(((AddExpr*)d)->rightNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(((ConstantExpr*)((AddExpr*)d)->leftNode())->evaluate(), double(1));
    EXPECT_EQ(((ConstantExpr*)((AddExpr*)d)->rightNode())->evaluate(), double(1));
}

// Constant * Constant
TEST(Expression, Multiply_Const_Const) {

}

// Constant * !Constant
TEST(Expression, Multiply_Const_Expr) {

}

// !Constant * Constant
TEST(Expression, Multiply_Expr_Const) {

}

// !Constant * !Constant
TEST(Expression, Multiply_Expr_Expr) {

}
