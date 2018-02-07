%{

#include <cstring>
#include "scanner.h"

// import parsers token type into local type def
typedef derivative::MathParser::token token;
typedef derivative::MathParser::token_type token_type;

// we want yylex to return token_type not int as usual
#define yyterminate() return token::END

// disables inclusion of unistd.h, which is not available in VS on Win32 
#define YY_NO_UNISTD_H

%}

/***** Flex Declarations and Options *****/

/* enable C++ scaner generation */
%option c++

/* change the name of scanner class => "DerivativeFlexLexer" */
%option prefix="Derivative"

/* manual => more optimized */
%option batch

/* enable scanner debug output => remove for release */
%option debug

/* no support for include files planned */
%option yywrap nounput

/* enables the use of start condition stacks */
%option stack

/* each time yylex is invoked the begin position is moved onto end position */
%{
#define YY_USER_ACTION yylloc->columns(yyleng);
%}

/* defintions for Regular Expressions */
digit [0-9]
integer {digit}+
exponent [eE][+-]?{integer}
real {integer}("."{integer})?{exponent}?
constant [A-Za-z]+

%% /***** Regular Expressions Part *****/


%{
    // reset location
    yylloc->step();
%}

{integer} {
    yylval->integerVal = atoi(yytext);
    return token::INTEGER;
}

{real} {
    yylval->doubleVal = atof(yytext);
    return token::DOUBLE;
}

"+" { return token::PLUS; }
"-" { return token::MINUS; }
"*" { return token::TIMES; }
"/" { return token::DIVIDE; }
"^" { return token::POWER; }
"(" { return token::LPARENT; }
")" { return token::RPARENT; }

"d/d" { return token::DERIVATIVE; }

"sin" { return token::SINFUNC; }
"cos" { return token::COSFUNC; }
"tan" { return token::TANFUNC; }
"exp" { return token::EXPFUNC; }
"ln" { return token::LNFUNC; }
"sqrt" { return token::SQRTFUNC; }

{constant} {
    yylval->stringVal = new std::string(yytext, yyleng);
    return token::STRING;
}

[ \t\r]+ {
    yylloc->step();
}

"\n" {
    yylloc->lines(yyleng);
    yylloc->step();
    return token::EOL;
}

. {
    return static_cast<token_type>(*yytext);
}

%% /*** Additional Code ***/

namespace derivative {

MathScanner::MathScanner(std::istream *in, std::ostream *out)
    : DerivativeFlexLexer(in, out)
{

}

MathScanner::~MathScanner()
{

}

void MathScanner::setDebug(bool b)
{
    yy_flex_debug = b;
}

}   // namespace derivative

/* This implementation of DerivativeFlexLexer::yylex() is required to fill the
 * vtable of the class DerivativeFlexLexer. We define the scanner's main yylex
 * function via YY_DECL to reside in the Scanner class instead. */

#ifdef yylex
#undef yylex
#endif

int DerivativeFlexLexer::yylex()
{
    std::cerr << "in DerivativeFlexLexer::yylex() !" << std::endl;
    return 0;
}

/* When the scanner receives an end-of-file indication from YY_INPUT, it then
 * checks the yywrap() function. If yywrap() returns false (zero), then it is
 * assumed that the function has gone ahead and set up `yyin' to point to
 * another input file, and scanning continues. If it returns true (non-zero),
 * then the scanner terminates, returning 0 to its caller. */

int DerivativeFlexLexer::yywrap()
{
    return 1;
}
