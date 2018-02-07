#include <gtest/gtest.h>

#include <iostream>

#include "expression.h"

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

// Constant * Constant
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

// Constant * !Constant
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

// !Constant * Constant
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

// !Constant * !Constant
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

TEST(DivideExpr, Derivative) {
    EXPECT_TRUE(false);
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

TEST(PowerExpr, Derivative) {
    EXPECT_EQ(true, false);
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

TEST(SinFunc, Derivative) {
    EXPECT_TRUE(false);
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

TEST(CosFunc, Derivative) {
    EXPECT_TRUE(false);
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

TEST(TanFunc, Derivative) {
    EXPECT_TRUE(false);
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
    EXPECT_TRUE(false);
}

TEST(LnFunc, Derivative) {
    EXPECT_TRUE(false);
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

TEST(SqrtFunc, Derivative) {
    EXPECT_TRUE(false);
}

