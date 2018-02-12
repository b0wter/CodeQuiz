#include <gtest/gtest.h>

#include <iostream>
#include <sstream>

#include "driver.h"
#include "expression.h"

class Derivatives : public ::testing::Test
{
public:
    Derivatives()
        : driver(context) {}

protected:
    virtual void SetUp() {}

    virtual void TearDown() {}

    void deriveAndCheck_EQ(char* input, char* expectedOutput)
    {                     
        std::ostringstream os_in;
        std::ostringstream os_out;
        std::ostringstream in_expr;
        std::ostringstream out_expr;
        context.clearExpressions();                 
        ASSERT_TRUE(driver.parseString(input, "input"));
        context.expressions[0]->print_expr(in_expr);
        context.expressions[0]->print_formula(os_in);
        ExprNode *d = context.expressions[0]->derivative();
        ExprNode *d2 = d->evaluate();
        d2->print_formula(os_out);
        d2->print_expr(out_expr);
        EXPECT_EQ(expectedOutput, os_out.str());
        delete d;
        delete d2;
    }

    ExprContext context;
    derivative::MathDriver driver;
};

TEST_F(Derivatives, F01) {
    deriveAndCheck_EQ("2x^10+2x^3", "20x^9+6x^2");
}

TEST_F(Derivatives, F02) {
    deriveAndCheck_EQ("2x^10 +2x^3", "20x^9+6x^2");
}

TEST_F(Derivatives, F03) {
    deriveAndCheck_EQ("2x^10 + 2x^3", "20x^9+6x^2");
}

TEST_F(Derivatives, F04) {
    deriveAndCheck_EQ("2x^10+ x^3", "20x^9+3x^2");
}

TEST_F(Derivatives, F05) {
    deriveAndCheck_EQ("2x^10 +2x ^3", "20x^9+6x^2");
}

TEST_F(Derivatives, F06) {
    deriveAndCheck_EQ("2x^10+2x^3+10", "20x^9+6x^2");
}

TEST_F(Derivatives, F07) {
    deriveAndCheck_EQ("2x^10+2x^3+sin(x)", "20x^9+6x^2+cos(x)");
}

TEST_F(Derivatives, F08) {
    // deriveAndCheck_EQ("2x^10(2x^3+10)", "");
}

TEST_F(Derivatives, F09) {
    // deriveAndCheck_EQ("2x^10(2x^3+10)", "");
}

TEST_F(Derivatives, F10) {
    deriveAndCheck_EQ("2x^-10", "-20x^-11");
}

TEST_F(Derivatives, F11) {

}