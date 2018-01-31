#ifndef DERIVATIVE_MATH_SCANNER_H
#define DERIVATIVE_MATH_SCANNER_H

// Flex expects the signature of yylex to be defined in the macro YY_DECL, and
// the C++ parser expects it to be declared. We can factor both as follows.

#ifndef YY_DECL

#define	YY_DECL						        \
    derivative::MathParser::token_type				\
    derivative::MathScanner::lex(				    \
	derivative::MathParser::semantic_type* yylval,	\
	derivative::MathParser::location_type* yylloc	\
    )
#endif

#ifndef __FLEX_LEXER_H
#define yyFlexLexer DerivativeFlexLexer
#include "FlexLexer.h"
#undef yyFlexLexer
#endif

#include "math_parser.tab.h"

namespace derivative {

class MathScanner : public DerivativeFlexLexer
{
public:
    MathScanner(std::istream* arg_yyin = 0,
                std::ostream* arg_yyout = 0);
    virtual ~MathScanner();

    virtual MathParser::token_type lex(MathParser::semantic_type* yylval,
                                       MathParser::location_type* yylloc);

    // enable debug output
    void setDebug(bool b);
};

}   // namespace derivative

#endif // DERIVATIVE_MATH_SCANNER_H