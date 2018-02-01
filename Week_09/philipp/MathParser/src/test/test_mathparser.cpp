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

    void testParseString_EQ(char * input, char* expectedOutput)
    {                     
        std::ostringstream os;                      
        context.clearExpressions();                 
        ASSERT_TRUE(driver.parseString(input, "input"));
        context.expressions[0]->print(os);          
        EXPECT_EQ(os.str(), expectedOutput);
    }

    void testParseString_NE(char * input, char* expectedOutput)
    {                     
        std::ostringstream os;                      
        context.clearExpressions();                 
        ASSERT_TRUE(driver.parseString(input, "input"));
        context.expressions[0]->print(os);          
        EXPECT_NE(os.str(), expectedOutput);
    }

    ExprContext context;
    derivative::MathDriver driver;
};                                                

TEST_F(ParserFixture, ConstantExpr) {
    testParseString_EQ("3", "C(3)");
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