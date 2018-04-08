#include <gtest/gtest.h>

#include <iostream>
#include <sstream>

#include "driver.h"
#include "expression.h"

class ParserFixture : public ::testing::Test
{
public:
    ParserFixture()
        : driver(context) {}

protected:
    virtual void SetUp()
    {
        
    }

    virtual void TearDown()
    {
        
    }

    void testParseString_EQ(char* input, char* expectedOutput)
    {                     
        std::ostringstream os;                      
        context.clearExpressions();                 
        ASSERT_TRUE(driver.parseString(input, "input"));
        context.expressions[0]->print_expr(os);          
        EXPECT_EQ(os.str(), expectedOutput);
    }

    void testParseString_NE(char* input, char* expectedOutput)
    {                     
        std::ostringstream os;                      
        context.clearExpressions();                 
        ASSERT_TRUE(driver.parseString(input, "input"));
        context.expressions[0]->print_expr(os);          
        EXPECT_NE(os.str(), expectedOutput);
    }

    ExprContext context;
    derivative::MathDriver driver;
};                                                

TEST_F(ParserFixture, ConstantExpr) {
    testParseString_EQ("3", "C(3)");
    testParseString_EQ("3.0", "C(3)");
    testParseString_EQ("3.2", "C(3.2)");
    testParseString_EQ("0.5", "C(0.5)");
}

TEST_F(ParserFixture, AddExpr) {
    testParseString_EQ("2 + 3", "ADD(C(2), C(3))");

    testParseString_NE("2 - 3", "ADD(C(2), C(3))");
}

TEST_F(ParserFixture, SubtractExpr) {
    testParseString_EQ("2-3", "SUB(C(2), C(3))");
    testParseString_EQ("2- 3", "SUB(C(2), C(3))");
    testParseString_EQ("2 -3", "SUB(C(2), C(3))");
    testParseString_EQ("2 - 3", "SUB(C(2), C(3))");
    testParseString_EQ("\t2 - 3", "SUB(C(2), C(3))");
    testParseString_EQ("2              - 3", "SUB(C(2), C(3))");
    testParseString_EQ("2 - 3\n\n", "SUB(C(2), C(3))");
    testParseString_EQ("2 -     \t 3", "SUB(C(2), C(3))");
    testParseString_EQ("22-33", "SUB(C(22), C(33))");
}

TEST_F(ParserFixture, MultiplyExpr) {
    testParseString_EQ("2 * 3", "MUL(C(2), C(3))");
}

TEST_F(ParserFixture, DivideExpr) {
    testParseString_EQ("4 / 5", "DIV(C(4), C(5))");
}

TEST_F(ParserFixture, PowerExpr) {
    testParseString_EQ("4 ^ 5", "POW(C(4), C(5))");
}

TEST_F(ParserFixture, SinFunc) {
    testParseString_EQ("sin(4+5)", "SIN(ADD(C(4), C(5)))");
}

TEST_F(ParserFixture, CosFunc) {
    testParseString_EQ("cos(10+11)", "COS(ADD(C(10), C(11)))");
}

TEST_F(ParserFixture, TanFunc) {
    testParseString_EQ("tan(19+25)", "TAN(ADD(C(19), C(25)))");
}

TEST_F(ParserFixture, ExpFunc) {
    testParseString_EQ("exp(-1+8)", "EXP(ADD((-)C(1), C(8)))");
}

TEST_F(ParserFixture, SqrtFunc) {
    testParseString_EQ("sqrt(9+10)", "SQRT(ADD(C(9), C(10)))");
}

TEST_F(ParserFixture, Expressions) {
    testParseString_EQ("2.3*exp(2 * a)", "MUL(C(2.3), EXP(MUL(C(2), P(a))))");
    testParseString_EQ("x", "V(x)");
    testParseString_EQ("2x", "MUL(C(2), V(x))");
    testParseString_EQ("2x^2", "MUL(C(2), POW(V(x), C(2)))");

    testParseString_EQ("2x^10+2x^3", "ADD(MUL(C(2), POW(V(x), C(10))), MUL(C(2), POW(V(x), C(3))))");
    /*testParseString_EQ("2x^10 +2x^3", "");
    testParseString_EQ("2x^10 + 2x^3", "");
    testParseString_EQ("2x^10+ x^3", "");
    testParseString_EQ("2x^10 +2x ^3", "");
    testParseString_EQ("2x^10+2x^3+10", "");
    testParseString_EQ("2x^10+2x^3+sin(x)", "");
    testParseString_EQ("2x^10(2x^3+10)", "");
    testParseString_EQ("2x^10(2x^3+10)", "");
    testParseString_EQ("2x^-10", "");*/
}