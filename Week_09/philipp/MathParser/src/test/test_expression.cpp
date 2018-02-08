#include <gtest/gtest.h>

#include <iostream>

#include "expression.h"

TEST(Expression, isNullExpr) {
    NullExpr e1;
    EXPECT_TRUE(e1.isNullExpr());
    ConstantExpr e2(2.0);
    EXPECT_FALSE(e2.isNullExpr());
    ParameterExpr e3("a");
    EXPECT_FALSE(e3.isNullExpr());
    VariableExpr e4("x");
    EXPECT_FALSE(e4.isNullExpr());
    NegateExpr e5(new SinFunc(new VariableExpr("x")));
    EXPECT_FALSE(e5.isNullExpr());
    AddExpr e6(new SinFunc(new VariableExpr("x")), new ConstantExpr(2.0));
    EXPECT_FALSE(e6.isNullExpr());
    SubtractExpr e7(new SinFunc(new VariableExpr("x")), new ConstantExpr(2.0));
    EXPECT_FALSE(e7.isNullExpr());
    MultiplyExpr e8(new SinFunc(new VariableExpr("x")), new ConstantExpr(2.0));
    EXPECT_FALSE(e8.isNullExpr());
    DivideExpr e9(new SinFunc(new VariableExpr("x")), new ConstantExpr(2.0));
    EXPECT_FALSE(e9.isNullExpr());
    PowerExpr e10(new SinFunc(new VariableExpr("x")), new ConstantExpr(2.0));
    EXPECT_FALSE(e10.isNullExpr());
    SinFunc e11(new SinFunc(new VariableExpr("x")));
    EXPECT_FALSE(e11.isNullExpr());
    CosFunc e12(new SinFunc(new VariableExpr("x")));
    EXPECT_FALSE(e12.isNullExpr());
    TanFunc e13(new SinFunc(new VariableExpr("x")));
    EXPECT_FALSE(e13.isNullExpr());
    ExpFunc e14(new SinFunc(new VariableExpr("x")));
    EXPECT_FALSE(e14.isNullExpr());
    LnFunc e15(new SinFunc(new VariableExpr("x")));
    EXPECT_FALSE(e15.isNullExpr());
    SqrtFunc e16(new SinFunc(new VariableExpr("x")));
    EXPECT_FALSE(e16.isNullExpr());
}

TEST(Expression, isConstExpr) {
    NullExpr e1;
    EXPECT_FALSE(e1.isConstExpr());
    ConstantExpr e2(2.0);
    EXPECT_TRUE(e2.isConstExpr());
    ParameterExpr e3("a");
    EXPECT_TRUE(e3.isConstExpr());
    VariableExpr e4("x");
    EXPECT_FALSE(e4.isConstExpr());
    NegateExpr e5(new SinFunc(new VariableExpr("x")));
    EXPECT_FALSE(e5.isConstExpr());
    AddExpr e6(new SinFunc(new VariableExpr("x")), new ConstantExpr(2.0));
    EXPECT_FALSE(e6.isConstExpr());
    SubtractExpr e7(new SinFunc(new VariableExpr("x")), new ConstantExpr(2.0));
    EXPECT_FALSE(e7.isConstExpr());
    MultiplyExpr e8(new SinFunc(new VariableExpr("x")), new ConstantExpr(2.0));
    EXPECT_FALSE(e8.isConstExpr());
    DivideExpr e9(new SinFunc(new VariableExpr("x")), new ConstantExpr(2.0));
    EXPECT_FALSE(e9.isConstExpr());
    PowerExpr e10(new SinFunc(new VariableExpr("x")), new ConstantExpr(2.0));
    EXPECT_FALSE(e10.isConstExpr());
    SinFunc e11(new SinFunc(new VariableExpr("x")));
    EXPECT_FALSE(e11.isConstExpr());
    CosFunc e12(new SinFunc(new VariableExpr("x")));
    EXPECT_FALSE(e12.isConstExpr());
    TanFunc e13(new SinFunc(new VariableExpr("x")));
    EXPECT_FALSE(e13.isConstExpr());
    ExpFunc e14(new SinFunc(new VariableExpr("x")));
    EXPECT_FALSE(e14.isConstExpr());
    LnFunc e15(new SinFunc(new VariableExpr("x")));
    EXPECT_FALSE(e15.isConstExpr());
    SqrtFunc e16(new SinFunc(new VariableExpr("x")));
    EXPECT_FALSE(e16.isConstExpr());
}

TEST(Expression, isVarExpr) {
    NullExpr e1;
    EXPECT_FALSE(e1.isVarExpr());
    ConstantExpr e2(2.0);
    EXPECT_FALSE(e2.isVarExpr());
    ParameterExpr e3("a");
    EXPECT_FALSE(e3.isVarExpr());
    VariableExpr e4("x");
    EXPECT_TRUE(e4.isVarExpr());
    NegateExpr e5(new SinFunc(new VariableExpr("x")));
    EXPECT_TRUE(e5.isVarExpr());
    AddExpr e6(new SinFunc(new VariableExpr("x")), new ConstantExpr(2.0));
    EXPECT_TRUE(e6.isVarExpr());
    SubtractExpr e7(new SinFunc(new VariableExpr("x")), new ConstantExpr(2.0));
    EXPECT_TRUE(e7.isVarExpr());
    MultiplyExpr e8(new SinFunc(new VariableExpr("x")), new ConstantExpr(2.0));
    EXPECT_TRUE(e8.isVarExpr());
    DivideExpr e9(new SinFunc(new VariableExpr("x")), new ConstantExpr(2.0));
    EXPECT_TRUE(e9.isVarExpr());
    PowerExpr e10(new SinFunc(new VariableExpr("x")), new ConstantExpr(2.0));
    EXPECT_TRUE(e10.isVarExpr());
    SinFunc e11(new SinFunc(new VariableExpr("x")));
    EXPECT_TRUE(e11.isVarExpr());
    CosFunc e12(new SinFunc(new VariableExpr("x")));
    EXPECT_TRUE(e12.isVarExpr());
    TanFunc e13(new SinFunc(new VariableExpr("x")));
    EXPECT_TRUE(e13.isVarExpr());
    ExpFunc e14(new SinFunc(new VariableExpr("x")));
    EXPECT_TRUE(e14.isVarExpr());
    LnFunc e15(new SinFunc(new VariableExpr("x")));
    EXPECT_TRUE(e15.isVarExpr());
    SqrtFunc e16(new SinFunc(new VariableExpr("x")));
    EXPECT_TRUE(e16.isVarExpr());
}

TEST(NullExpr, Copy) {
    NullExpr *n1 = new NullExpr();
    NullExpr *n2 = n1;
    EXPECT_EQ(n1, n2);
    n2 = (NullExpr*)n1->copy();
    EXPECT_NE(n1, n2);
    delete n1;
    delete n2;
}

TEST(NullExpr, Derivative) {
    std::ostringstream os;
    NullExpr n;
    EXPECT_EQ(n.evaluate(), DBL_MAX);
    EXPECT_EQ(n.derivative()->type(), NodeTypes::NullExpr);
    EXPECT_EQ(n.type(), NodeTypes::NullExpr);
    n.print(os, 0);
    EXPECT_EQ(os.str(), "");
}

TEST(ConstantExpr, Copy) {
    ConstantExpr *c1 = new ConstantExpr(2);
    ConstantExpr *c2 = c1;
    EXPECT_EQ(c1, c2);
    c2 = (ConstantExpr*)c1->copy();
    EXPECT_NE(c1, c2);
    EXPECT_DOUBLE_EQ(c1->evaluate(), c2->evaluate());
    delete c1;
    delete c2;
}

TEST(ConstantExpr, Derivative) {
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

TEST(ParameterExpr, Copy) {
    ParameterExpr *p1 = new ParameterExpr("y");
    ParameterExpr *p2 = p1;
    EXPECT_EQ(p1, p2);
    p2 = (ParameterExpr*)p1->copy();
    EXPECT_NE(p1, p2);
    EXPECT_EQ(p1->value(), p2->value());
    delete p1;
    delete p2;
}

TEST(ParameterExpr, Derivative) {
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

TEST(VariableExpr, Copy) {
    VariableExpr *v1 = nullptr;
    v1 = new VariableExpr("y");
    VariableExpr *v2 = nullptr;
    v2 = v1;
    EXPECT_EQ(v1, v2);
    v2 = (VariableExpr*)v1->copy();
    EXPECT_NE(v1, v2);
    EXPECT_EQ(v1->value(), v2->value());
    delete v1;
    delete v2;
}

TEST(VariableExpr, Derivative) {
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

TEST(NegateExpr, Copy) {
    NegateExpr *n1 = nullptr;
    n1 = new NegateExpr(new ConstantExpr(2.0));
    NegateExpr *n2 = nullptr;
    n2 = n1;
    EXPECT_EQ(n1, n2);
    EXPECT_EQ(n1->innerNode(), n2->innerNode());
    n2 = (NegateExpr*)n1->copy();
    EXPECT_NE(n1, n2);
    EXPECT_NE(n1->innerNode(), n2->innerNode());
    delete n1;
    delete n2;
}

TEST(NegateExpr, Derivative) {
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

TEST(AddExpr, Copy) {
    AddExpr *a1 = nullptr;
    a1 = new AddExpr(new ConstantExpr(2.0), new ConstantExpr(4.0));
    AddExpr *a2 = nullptr;
    a2 = a1;
    EXPECT_EQ(a1, a2);
    EXPECT_EQ(a1->leftNode(), a2->leftNode());
    EXPECT_EQ(a1->rightNode(), a2->rightNode());
    a2 = (AddExpr*)a1->copy();
    EXPECT_NE(a1, a2);
    EXPECT_NE(a1->leftNode(), a2->leftNode());
    EXPECT_NE(a1->rightNode(), a2->rightNode());
    delete a1;
    delete a2;
}

// Constant + Constant
TEST(AddExpr, Derivative_Const_Const) {
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
TEST(AddExpr, Derivative_Const_Var) {
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
TEST(AddExpr, Derivative_Var_Const) {
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
TEST(AddExpr, Derivative_Var_Var) {
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

TEST(SubtractExpr, Copy) {
    SubtractExpr *s1 = nullptr;
    s1 = new SubtractExpr(new ConstantExpr(2.0), new ConstantExpr(4.0));
    SubtractExpr *s2 = nullptr;
    s2 = s1;
    EXPECT_EQ(s1, s2);
    EXPECT_EQ(s1->leftNode(), s2->leftNode());
    EXPECT_EQ(s1->rightNode(), s2->rightNode());
    s2 = (SubtractExpr*)s1->copy();
    EXPECT_NE(s1, s2);
    EXPECT_NE(s1->leftNode(), s2->leftNode());
    EXPECT_NE(s1->rightNode(), s2->rightNode());
    delete s1;
    delete s2;
}

// Constant - Constant
TEST(SubtractExpr, Derivative_Const_Const) {
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
TEST(SubtractExpr, Derivative_Const_Var) {
    std::ostringstream os;
    SubtractExpr a(new ConstantExpr(2), new VariableExpr("x"));
    EXPECT_EQ(a.type(), NodeTypes::SubtractExpr);
    EXPECT_EQ(a.leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(a.rightNode()->type(), NodeTypes::VariableExpr);
    a.print(os, 0);
    EXPECT_EQ(os.str(), "SUB(C(2), V(x))");
    ExprNode *d = a.derivative();
    ASSERT_EQ(d->type(), NodeTypes::NegateExpr);
    EXPECT_EQ(((NegateExpr*)d)->innerNode()->type(), NodeTypes::ConstantExpr);
}

// !Constant - Constant
TEST(SubtractExpr, Derivative_Var_Const) {
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
TEST(SubtractExpr, Derivative_Var_Var) {
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

TEST(MultiplyExpr, Copy) {
    MultiplyExpr *m1 = nullptr;
    m1 = new MultiplyExpr(new ConstantExpr(2.0), new ConstantExpr(4.0));
    MultiplyExpr *m2 = nullptr;
    m2 = m1;
    EXPECT_EQ(m1, m2);
    EXPECT_EQ(m1->leftNode(), m2->leftNode());
    EXPECT_EQ(m1->rightNode(), m2->rightNode());
    m2 = (MultiplyExpr*)m1->copy();
    EXPECT_NE(m1, m2);
    EXPECT_NE(m1->leftNode(), m2->leftNode());
    EXPECT_NE(m1->rightNode(), m2->rightNode());
    delete m1;
    delete m2;
}

TEST(MultiplyExpr, Derivative_Const_Const) {
    std::ostringstream os;
    MultiplyExpr m(new ConstantExpr(1), new ConstantExpr(2));
    EXPECT_EQ(m.type(), NodeTypes::MultiplyExpr);
    EXPECT_EQ(m.leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(m.rightNode()->type(), NodeTypes::ConstantExpr);
    m.print(os, 0);
    EXPECT_EQ(os.str(), "MUL(C(1), C(2))");
    ExprNode *d = m.derivative();
    ASSERT_EQ(d->type(), NodeTypes::NullExpr);
}

TEST(MultiplyExpr, Derivative_Const_Expr) {
    std::ostringstream os;
    MultiplyExpr m(new ConstantExpr(1), new VariableExpr("x"));
    EXPECT_EQ(m.type(), NodeTypes::MultiplyExpr);
    EXPECT_EQ(m.leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(m.rightNode()->type(), NodeTypes::VariableExpr);
    m.print(os, 0);
    EXPECT_EQ(os.str(), "MUL(C(1), V(x))");
    ExprNode *d = m.derivative();
    ASSERT_EQ(d->type(), NodeTypes::MultiplyExpr);
    ASSERT_EQ(((MultiplyExpr*)d)->leftNode()->type(), NodeTypes::ConstantExpr);
    ASSERT_EQ(((MultiplyExpr*)d)->rightNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(((ConstantExpr*)((MultiplyExpr*)d)->leftNode())->evaluate(), double(1));
    EXPECT_EQ(((ConstantExpr*)((MultiplyExpr*)d)->rightNode())->evaluate(), double(1));
}

TEST(MultiplyExpr, Derivative_Expr_Const) {
    std::ostringstream os;
    MultiplyExpr m(new VariableExpr("x"), new ConstantExpr(2));
    EXPECT_EQ(m.type(), NodeTypes::MultiplyExpr);
    EXPECT_EQ(m.leftNode()->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(m.rightNode()->type(), NodeTypes::ConstantExpr);
    m.print(os, 0);
    EXPECT_EQ(os.str(), "MUL(V(x), C(2))");
    ExprNode *d = m.derivative();
    ASSERT_EQ(d->type(), NodeTypes::MultiplyExpr);
    ASSERT_EQ(((MultiplyExpr*)d)->leftNode()->type(), NodeTypes::ConstantExpr);
    ASSERT_EQ(((MultiplyExpr*)d)->rightNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(((ConstantExpr*)((MultiplyExpr*)d)->leftNode())->evaluate(), double(1));
    EXPECT_EQ(((ConstantExpr*)((MultiplyExpr*)d)->rightNode())->evaluate(), double(2));
}

TEST(MultiplyExpr, Derivative_Expr_Expr) {
    std::ostringstream os;
    MultiplyExpr m(new VariableExpr("x"), new VariableExpr("x"));
    EXPECT_EQ(m.type(), NodeTypes::MultiplyExpr);
    EXPECT_EQ(m.leftNode()->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(m.rightNode()->type(), NodeTypes::VariableExpr);
    m.print(os, 0);
    EXPECT_EQ(os.str(), "MUL(V(x), V(x))");
    ExprNode *d = m.derivative();
    ASSERT_EQ(d->type(), NodeTypes::AddExpr);
    ASSERT_EQ(((AddExpr*)d)->leftNode()->type(), NodeTypes::MultiplyExpr);
    ASSERT_EQ(((AddExpr*)d)->rightNode()->type(), NodeTypes::MultiplyExpr);
    EXPECT_EQ(((ConstantExpr*)((MultiplyExpr*)((AddExpr*)d)->leftNode())->leftNode())->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(((VariableExpr*)((MultiplyExpr*)((AddExpr*)d)->leftNode())->rightNode())->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(((VariableExpr*)((MultiplyExpr*)((AddExpr*)d)->rightNode())->leftNode())->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(((ConstantExpr*)((MultiplyExpr*)((AddExpr*)d)->rightNode())->rightNode())->type(), NodeTypes::ConstantExpr);
}

TEST(DivideExpr, Copy) {
    DivideExpr *d1 = nullptr;
    d1 = new DivideExpr(new ConstantExpr(2.0), new ConstantExpr(4.0));
    DivideExpr *d2 = nullptr;
    d2 = d1;
    EXPECT_EQ(d1, d2);
    EXPECT_EQ(d1->leftNode(), d2->leftNode());
    EXPECT_EQ(d1->rightNode(), d2->rightNode());
    d2 = (DivideExpr*)d1->copy();
    EXPECT_NE(d1, d2);
    EXPECT_NE(d1->leftNode(), d2->leftNode());
    EXPECT_NE(d1->rightNode(), d2->rightNode());
    delete d1;
    delete d2;
}

TEST(DivideExpr, Derivative_Const_Const) {
    std::ostringstream os;
    DivideExpr e(new ConstantExpr(1), new ConstantExpr(2));
    EXPECT_EQ(e.type(), NodeTypes::DivideExpr);
    EXPECT_EQ(e.leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(e.rightNode()->type(), NodeTypes::ConstantExpr);
    e.print(os, 0);
    EXPECT_EQ(os.str(), "DIV(C(1), C(2))");
    ExprNode *d = e.derivative();
    ASSERT_EQ(d->type(), NodeTypes::NullExpr);
}

TEST(DivideExpr, Derivative_Const_Expr) {
    std::ostringstream os;
    DivideExpr e(new ConstantExpr(1), new VariableExpr("x"));
    EXPECT_EQ(e.type(), NodeTypes::DivideExpr);
    EXPECT_EQ(e.leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(e.rightNode()->type(), NodeTypes::VariableExpr);
    e.print(os, 0);
    EXPECT_EQ(os.str(), "DIV(C(1), V(x))");
    ExprNode *d = e.derivative();
    ASSERT_EQ(d->type(), NodeTypes::MultiplyExpr);
    ASSERT_EQ(((MultiplyExpr*)d)->leftNode()->type(), NodeTypes::ConstantExpr);
    ASSERT_EQ(((MultiplyExpr*)d)->rightNode()->type(), NodeTypes::DivideExpr);
    EXPECT_EQ(((ConstantExpr*)((MultiplyExpr*)d)->leftNode())->evaluate(), double(1));
    EXPECT_EQ(((DivideExpr*)((MultiplyExpr*)d)->rightNode())->leftNode()->type(), NodeTypes::NegateExpr);
    EXPECT_EQ(((DivideExpr*)((MultiplyExpr*)d)->rightNode())->rightNode()->type(), NodeTypes::PowerExpr);
    EXPECT_EQ(((PowerExpr*)((DivideExpr*)((MultiplyExpr*)d)->rightNode())->rightNode())->leftNode()->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(((PowerExpr*)((DivideExpr*)((MultiplyExpr*)d)->rightNode())->rightNode())->rightNode()->type(), NodeTypes::ConstantExpr);
}

TEST(DivideExpr, Derivative_Expr_Const) {
    std::ostringstream os;
    DivideExpr e(new VariableExpr("x"), new ConstantExpr(2.0));
    EXPECT_EQ(e.type(), NodeTypes::DivideExpr);
    EXPECT_EQ(e.leftNode()->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(e.rightNode()->type(), NodeTypes::ConstantExpr);
    e.print(os, 0);
    EXPECT_EQ(os.str(), "DIV(V(x), C(2))");
    ExprNode *d = e.derivative();
    ASSERT_EQ(d->type(), NodeTypes::MultiplyExpr);
    ASSERT_EQ(((MultiplyExpr*)d)->leftNode()->type(), NodeTypes::DivideExpr);
    ASSERT_EQ(((MultiplyExpr*)d)->rightNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(((ConstantExpr*)((MultiplyExpr*)d)->rightNode())->evaluate(), double(1));
    EXPECT_EQ(((DivideExpr*)((MultiplyExpr*)d)->leftNode())->leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(((DivideExpr*)((MultiplyExpr*)d)->leftNode())->rightNode()->type(), NodeTypes::ConstantExpr);
}

TEST(DivideExpr, Derivative_Expr_Expr) {
    std::ostringstream os;
    DivideExpr e(new AddExpr(new VariableExpr("x"), new ConstantExpr(2.0)),
                 new PowerExpr(new VariableExpr("x"), new ConstantExpr(3.0)));
    EXPECT_EQ(e.type(), NodeTypes::DivideExpr);
    EXPECT_EQ(e.leftNode()->type(), NodeTypes::AddExpr);
    EXPECT_EQ(e.rightNode()->type(), NodeTypes::PowerExpr);
    e.print(os, 0);
    EXPECT_EQ(os.str(), "DIV(ADD(V(x), C(2)), POW(V(x), C(3)))");
    ExprNode *d = e.derivative();
    ASSERT_EQ(d->type(), NodeTypes::DivideExpr);
    ASSERT_EQ(((DivideExpr*)d)->leftNode()->type(), NodeTypes::SubtractExpr);
    SubtractExpr *s = (SubtractExpr *)((DivideExpr*)d)->leftNode();
    ASSERT_EQ(((DivideExpr*)d)->rightNode()->type(), NodeTypes::PowerExpr);
    PowerExpr *p = (PowerExpr *)((DivideExpr*)d)->rightNode();
    ASSERT_EQ(s->leftNode()->type(), NodeTypes::MultiplyExpr);
    MultiplyExpr *m1 = (MultiplyExpr*)s->leftNode();
    EXPECT_EQ(m1->leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(m1->rightNode()->type(), NodeTypes::PowerExpr);
    ASSERT_EQ(s->rightNode()->type(), NodeTypes::MultiplyExpr);
    MultiplyExpr *m2 = (MultiplyExpr*)s->rightNode();
    EXPECT_EQ(m2->leftNode()->type(), NodeTypes::AddExpr);
    EXPECT_EQ(m2->rightNode()->type(), NodeTypes::MultiplyExpr);
    ASSERT_EQ(p->leftNode()->type(), NodeTypes::PowerExpr);
    EXPECT_EQ(((PowerExpr*)p->leftNode())->leftNode()->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(((PowerExpr*)p->leftNode())->rightNode()->type(), NodeTypes::ConstantExpr);
    ASSERT_EQ(p->rightNode()->type(), NodeTypes::ConstantExpr);
}

TEST(PowerExpr, Copy) {
    PowerExpr *p1 = nullptr;
    p1 = new PowerExpr(new ConstantExpr(2.0), new ConstantExpr(4.0));
    PowerExpr *p2 = nullptr;
    p2 = p1;
    EXPECT_EQ(p1, p2);
    EXPECT_EQ(p1->leftNode(), p2->leftNode());
    EXPECT_EQ(p1->rightNode(), p2->rightNode());
    p2 = (PowerExpr*)p1->copy();
    EXPECT_NE(p1, p2);
    EXPECT_NE(p1->leftNode(), p2->leftNode());
    EXPECT_NE(p1->rightNode(), p2->rightNode());
    delete p1;
    delete p2;
}

TEST(PowerExpr, Derivative_Const_Const) {
    std::ostringstream os;
    PowerExpr e(new ConstantExpr(1), new ConstantExpr(2));
    EXPECT_EQ(e.type(), NodeTypes::PowerExpr);
    EXPECT_EQ(e.leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(e.rightNode()->type(), NodeTypes::ConstantExpr);
    e.print(os, 0);
    EXPECT_EQ(os.str(), "POW(C(1), C(2))");
    ExprNode *d = e.derivative();
    ASSERT_EQ(d->type(), NodeTypes::NullExpr);
}

TEST(PowerExpr, Derivative_Expr_Const) {
    std::ostringstream os;
    PowerExpr e(new VariableExpr("x"), new ConstantExpr(2));
    EXPECT_EQ(e.type(), NodeTypes::PowerExpr);
    EXPECT_EQ(e.leftNode()->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(e.rightNode()->type(), NodeTypes::ConstantExpr);
    e.print(os, 0);
    EXPECT_EQ(os.str(), "POW(V(x), C(2))");
    ExprNode *d = e.derivative();
    ASSERT_EQ(d->type(), NodeTypes::MultiplyExpr);
    EXPECT_EQ(((MultiplyExpr*)d)->leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(((MultiplyExpr*)d)->rightNode()->type(), NodeTypes::PowerExpr);
    EXPECT_EQ(((PowerExpr*)((MultiplyExpr*)d)->rightNode())->leftNode()->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(((PowerExpr*)((MultiplyExpr*)d)->rightNode())->rightNode()->type(), NodeTypes::SubtractExpr);
    EXPECT_EQ(((SubtractExpr*)((PowerExpr*)((MultiplyExpr*)d)->rightNode())->rightNode())->leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(((SubtractExpr*)((PowerExpr*)((MultiplyExpr*)d)->rightNode())->rightNode())->rightNode()->type(), NodeTypes::ConstantExpr);
}

TEST(PowerExpr, Derivative_Expr_Expr) {
    std::ostringstream os;
    PowerExpr e(new VariableExpr("x"), new SinFunc(new VariableExpr("x")));
    EXPECT_EQ(e.type(), NodeTypes::PowerExpr);
    EXPECT_EQ(e.leftNode()->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(e.rightNode()->type(), NodeTypes::SinFunc);
    e.print(os, 0);
    EXPECT_EQ(os.str(), "POW(V(x), SIN(V(x)))");
    ExprNode *d = e.derivative();
    ASSERT_EQ(d->type(), NodeTypes::MultiplyExpr);
    EXPECT_EQ(((MultiplyExpr*)d)->leftNode()->type(), NodeTypes::PowerExpr);
    EXPECT_EQ(((MultiplyExpr*)d)->rightNode()->type(), NodeTypes::AddExpr);
    EXPECT_EQ(((PowerExpr*)((MultiplyExpr*)d)->leftNode())->leftNode()->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(((PowerExpr*)((MultiplyExpr*)d)->leftNode())->rightNode()->type(), NodeTypes::SinFunc);
    EXPECT_EQ(((AddExpr*)((MultiplyExpr*)d)->rightNode())->leftNode()->type(), NodeTypes::MultiplyExpr);
    EXPECT_EQ(((AddExpr*)((MultiplyExpr*)d)->rightNode())->rightNode()->type(), NodeTypes::MultiplyExpr);
    EXPECT_EQ(((MultiplyExpr*)((AddExpr*)((MultiplyExpr*)d)->rightNode())->leftNode())->leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(((MultiplyExpr*)((AddExpr*)((MultiplyExpr*)d)->rightNode())->leftNode())->rightNode()->type(), NodeTypes::LnFunc);
    EXPECT_EQ(((LnFunc*)((MultiplyExpr*)((AddExpr*)((MultiplyExpr*)d)->rightNode())->leftNode())->rightNode())->innerNode()->type(), NodeTypes::CosFunc);
    EXPECT_EQ(((MultiplyExpr*)((AddExpr*)((MultiplyExpr*)d)->rightNode())->rightNode())->leftNode()->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(((MultiplyExpr*)((AddExpr*)((MultiplyExpr*)d)->rightNode())->rightNode())->rightNode()->type(), NodeTypes::DivideExpr);
    EXPECT_EQ(((DivideExpr*)((MultiplyExpr*)((AddExpr*)((MultiplyExpr*)d)->rightNode())->rightNode())->rightNode())->leftNode()->type(), NodeTypes::CosFunc);
    EXPECT_EQ(((DivideExpr*)((MultiplyExpr*)((AddExpr*)((MultiplyExpr*)d)->rightNode())->rightNode())->rightNode())->rightNode()->type(), NodeTypes::SinFunc);
}

TEST(SinFunc, Copy) {
    SinFunc *f1 = nullptr;
    f1 = new SinFunc(new ConstantExpr(2.0));
    SinFunc *f2 = nullptr;
    f2 = f1;
    EXPECT_EQ(f1, f2);
    EXPECT_EQ(f1->innerNode(), f2->innerNode());
    f2 = (SinFunc*)f1->copy();
    EXPECT_NE(f1, f2);
    EXPECT_NE(f1->innerNode(), f2->innerNode());
    delete f1;
    delete f2;
}

TEST(SinFunc, Derivative_Const) {
    std::ostringstream os1;
    SinFunc f(new ConstantExpr(2.0));
    EXPECT_EQ(f.type(), NodeTypes::SinFunc);
    EXPECT_EQ(f.innerNode()->type(), NodeTypes::ConstantExpr);
    f.print(os1, 0);
    EXPECT_EQ(os1.str(), "SIN(C(2))");
    ExprNode *d = f.derivative();
    EXPECT_EQ(d->type(), NodeTypes::NullExpr);
}

TEST(SinFunc, Derivative_Var) {
    std::ostringstream os1;
    SinFunc f(new VariableExpr("x"));
    EXPECT_EQ(f.type(), NodeTypes::SinFunc);
    EXPECT_EQ(f.innerNode()->type(), NodeTypes::VariableExpr);
    f.print(os1, 0);
    EXPECT_EQ(os1.str(), "SIN(V(x))");
    ExprNode *d = f.derivative();
    EXPECT_EQ(d->type(), NodeTypes::CosFunc);
    EXPECT_EQ(((CosFunc*)d)->innerNode()->type(), NodeTypes::VariableExpr);
    std::ostringstream os2;
    d->print(os2, 0);
    EXPECT_EQ(os2.str(), "COS(V(x))");
}

TEST(SinFunc, Derivative_Expr) {
    std::ostringstream os1;
    SinFunc f(new AddExpr(new ConstantExpr(2.0), new VariableExpr("x")));
    EXPECT_EQ(f.type(), NodeTypes::SinFunc);
    EXPECT_EQ(f.innerNode()->type(), NodeTypes::AddExpr);
    f.print(os1, 0);
    EXPECT_EQ(os1.str(), "SIN(ADD(C(2), V(x)))");
    ExprNode *d = f.derivative();
    EXPECT_EQ(d->type(), NodeTypes::MultiplyExpr);
    EXPECT_EQ(((MultiplyExpr*)d)->leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(((MultiplyExpr*)d)->rightNode()->type(), NodeTypes::CosFunc);
    std::ostringstream os2;
    d->print(os2, 0);
    EXPECT_EQ(os2.str(), "MUL(C(1), COS(ADD(C(2), V(x))))");
}

TEST(CosFunc, Copy) {
    CosFunc *f1 = nullptr;
    f1 = new CosFunc(new ConstantExpr(2.0));
    CosFunc *f2 = nullptr;
    f2 = f1;
    EXPECT_EQ(f1, f2);
    EXPECT_EQ(f1->innerNode(), f2->innerNode());
    f2 = (CosFunc*)f1->copy();
    EXPECT_NE(f1, f2);
    EXPECT_NE(f1->innerNode(), f2->innerNode());
    delete f1;
    delete f2;
}

TEST(CosFunc, Derivative_Const) {
    std::ostringstream os1;
    CosFunc f(new ConstantExpr(2.0));
    EXPECT_EQ(f.type(), NodeTypes::CosFunc);
    EXPECT_EQ(f.innerNode()->type(), NodeTypes::ConstantExpr);
    f.print(os1, 0);
    EXPECT_EQ(os1.str(), "COS(C(2))");
    ExprNode *d = f.derivative();
    EXPECT_EQ(d->type(), NodeTypes::NullExpr);
}

TEST(CosFunc, Derivative_Var) {
    std::ostringstream os1;
    CosFunc f(new VariableExpr("x"));
    EXPECT_EQ(f.type(), NodeTypes::CosFunc);
    EXPECT_EQ(f.innerNode()->type(), NodeTypes::VariableExpr);
    f.print(os1, 0);
    EXPECT_EQ(os1.str(), "COS(V(x))");
    ExprNode *d = f.derivative();
    EXPECT_EQ(d->type(), NodeTypes::NegateExpr);
    EXPECT_EQ(((NegateExpr*)d)->innerNode()->type(), NodeTypes::SinFunc);
    EXPECT_EQ(((CosFunc*)((NegateExpr*)d)->innerNode())->innerNode()->type(), NodeTypes::VariableExpr);
    std::ostringstream os2;
    d->print(os2, 0);
    EXPECT_EQ(os2.str(), "(-)SIN(V(x))");
}

TEST(CosFunc, Derivative_Expr) {
    std::ostringstream os1;
    CosFunc f(new AddExpr(new ConstantExpr(2.0), new VariableExpr("x")));
    EXPECT_EQ(f.type(), NodeTypes::CosFunc);
    EXPECT_EQ(f.innerNode()->type(), NodeTypes::AddExpr);
    f.print(os1, 0);
    EXPECT_EQ(os1.str(), "COS(ADD(C(2), V(x)))");
    ExprNode *d = f.derivative();
    EXPECT_EQ(d->type(), NodeTypes::NegateExpr);
    EXPECT_EQ(((MultiplyExpr*)((NegateExpr*)d)->innerNode())->leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(((MultiplyExpr*)((NegateExpr*)d)->innerNode())->rightNode()->type(), NodeTypes::SinFunc);
    std::ostringstream os2;
    d->print(os2, 0);
    EXPECT_EQ(os2.str(), "(-)MUL(C(1), SIN(ADD(C(2), V(x))))");
}

TEST(TanFunc, Copy) {
    TanFunc *f1 = nullptr;
    f1 = new TanFunc(new ConstantExpr(2.0));
    TanFunc *f2 = nullptr;
    f2 = f1;
    EXPECT_EQ(f1, f2);
    EXPECT_EQ(f1->innerNode(), f2->innerNode());
    f2 = (TanFunc*)f1->copy();
    EXPECT_NE(f1, f2);
    EXPECT_NE(f1->innerNode(), f2->innerNode());
    delete f1;
    delete f2;
}

TEST(TanFunc, Derivative_Const) {
    std::ostringstream os1;
    TanFunc f(new ConstantExpr(2.0));
    EXPECT_EQ(f.type(), NodeTypes::TanFunc);
    EXPECT_EQ(f.innerNode()->type(), NodeTypes::ConstantExpr);
    f.print(os1, 0);
    EXPECT_EQ(os1.str(), "TAN(C(2))");
    ExprNode *d = f.derivative();
    EXPECT_EQ(d->type(), NodeTypes::NullExpr);
}

TEST(TanFunc, Derivative_Expr) {
    std::ostringstream os1;
    TanFunc f(new AddExpr(new ConstantExpr(2.0), new VariableExpr("x")));
    EXPECT_EQ(f.type(), NodeTypes::TanFunc);
    EXPECT_EQ(f.innerNode()->type(), NodeTypes::AddExpr);
    f.print(os1, 0);
    EXPECT_EQ(os1.str(), "TAN(ADD(C(2), V(x)))");
    ExprNode *d = f.derivative();
    EXPECT_EQ(d->type(), NodeTypes::DivideExpr);
    EXPECT_EQ(((DivideExpr*)d)->leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(((PowerExpr*)((DivideExpr*)d)->rightNode())->leftNode()->type(), NodeTypes::CosFunc);
    EXPECT_EQ(((PowerExpr*)((DivideExpr*)d)->rightNode())->rightNode()->type(), NodeTypes::ConstantExpr);
    std::ostringstream os2;
    d->print(os2, 0);
    EXPECT_EQ(os2.str(), "DIV(C(1), POW(COS(ADD(C(2), V(x))), C(2)))");
}

TEST(ExpFunc, Copy) {
    ExpFunc *f1 = nullptr;
    f1 = new ExpFunc(new ConstantExpr(2.0));
    ExpFunc *f2 = nullptr;
    f2 = f1;
    EXPECT_EQ(f1, f2);
    EXPECT_EQ(f1->innerNode(), f2->innerNode());
    f2 = (ExpFunc*)f1->copy();
    EXPECT_NE(f1, f2);
    EXPECT_NE(f1->innerNode(), f2->innerNode());
    delete f1;
    delete f2;
}

TEST(ExpFunc, Derivative) {
    ExpFunc *f1 = nullptr;
    ExprNode *f1_d = nullptr;
    f1 = new ExpFunc(new ConstantExpr(2));
    f1_d = f1->derivative();
    EXPECT_EQ(f1_d->type(), NodeTypes::NullExpr);
    delete f1;
    delete f1_d;
    f1 = new ExpFunc(new ParameterExpr("d"));
    f1_d = f1->derivative();
    EXPECT_EQ(f1_d->type(), NodeTypes::NullExpr);
    delete f1;
    delete f1_d;
    f1 = new ExpFunc(new VariableExpr("x"));
    f1_d = f1->derivative();
    EXPECT_EQ(f1_d->type(), NodeTypes::ExpFunc);
    EXPECT_EQ(((ExpFunc*)f1_d)->innerNode()->type(), NodeTypes::VariableExpr);
    EXPECT_EQ(((VariableExpr*)((ExpFunc*)f1_d)->innerNode())->value(), "x");
    delete f1;
    delete f1_d;
    // TODO: Test more Complex Functions, e.g SinFunc
}

TEST(LnFunc, Copy) {
    LnFunc *f1 = nullptr;
    f1 = new LnFunc(new ConstantExpr(2.0));
    LnFunc *f2 = nullptr;
    f2 = f1;
    EXPECT_EQ(f1, f2);
    EXPECT_EQ(f1->innerNode(), f2->innerNode());
    f2 = (LnFunc*)f1->copy();
    EXPECT_NE(f1, f2);
    EXPECT_NE(f1->innerNode(), f2->innerNode());
    delete f1;
    delete f2; 
}

TEST(LnFunc, Derivative_Const) {
    std::ostringstream os;
    LnFunc ln = LnFunc(new ConstantExpr(2.0));
    EXPECT_EQ(ln.type(), NodeTypes::LnFunc);
    EXPECT_EQ(ln.innerNode()->type(), NodeTypes::ConstantExpr);
    ln.print(os, 0);
    EXPECT_EQ(os.str(), "LN(C(2))");
    ExprNode *d = ln.derivative();
    EXPECT_EQ(d->type(), NodeTypes::NullExpr);
}

TEST(LnFunc, Derivative_Expr) {
    std::ostringstream os;
    LnFunc ln = LnFunc(new AddExpr(new ConstantExpr(2.0),
                                   new VariableExpr("x")));
    EXPECT_EQ(ln.type(), NodeTypes::LnFunc);
    EXPECT_EQ(ln.innerNode()->type(), NodeTypes::AddExpr);
    ln.print(os, 0);
    EXPECT_EQ(os.str(), "LN(ADD(C(2), V(x)))");
    ExprNode *d = ln.derivative();
    EXPECT_EQ(d->type(), NodeTypes::DivideExpr);
    EXPECT_EQ(((DivideExpr*)d)->leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(((DivideExpr*)d)->rightNode()->type(), NodeTypes::AddExpr);
}

TEST(SqrtFunc, Copy) {
    SqrtFunc *f1 = nullptr;
    f1 = new SqrtFunc(new ConstantExpr(2.0));
    SqrtFunc *f2 = nullptr;
    f2 = f1;
    EXPECT_EQ(f1, f2);
    EXPECT_EQ(f1->innerNode(), f2->innerNode());
    f2 = (SqrtFunc*)f1->copy();
    EXPECT_NE(f1, f2);
    EXPECT_NE(f1->innerNode(), f2->innerNode());
    delete f1;
    delete f2;
}

TEST(SqrtFunc, Derivative_Const) {
    std::ostringstream os;
    SqrtFunc s = SqrtFunc(new ConstantExpr(2.0));
    EXPECT_EQ(s.type(), NodeTypes::SqrtFunc);
    EXPECT_EQ(s.innerNode()->type(), NodeTypes::ConstantExpr);
    s.print(os, 0);
    EXPECT_EQ(os.str(), "SQRT(C(2))");
    ExprNode *d = s.derivative();
    EXPECT_EQ(d->type(), NodeTypes::NullExpr);
}

TEST(SqrtFunc, Derivative_Expr) {
    std::ostringstream os;
    SqrtFunc s = SqrtFunc(new VariableExpr("x"));
    EXPECT_EQ(s.type(), NodeTypes::SqrtFunc);
    EXPECT_EQ(s.innerNode()->type(), NodeTypes::VariableExpr);
    s.print(os, 0);
    EXPECT_EQ(os.str(), "SQRT(V(x))");
    ExprNode *d = s.derivative();
    ASSERT_EQ(d->type(), NodeTypes::DivideExpr);
    EXPECT_EQ(((DivideExpr*)d)->leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(((DivideExpr*)d)->rightNode()->type(), NodeTypes::MultiplyExpr);
    MultiplyExpr *m = (MultiplyExpr*)((DivideExpr*)d)->rightNode();
    EXPECT_EQ(m->leftNode()->type(), NodeTypes::ConstantExpr);
    EXPECT_EQ(m->rightNode()->type(), NodeTypes::SqrtFunc);
}