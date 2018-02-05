%{

#include <cstdio>
#include <string>
#include <vector>

#include "expression.h"

%}

/***** Bison Declarations *****/

// Bison 2.3 or later required
%require "2.3"

// add debug output code to generated parser
%debug

// start symbol should be start
%start start

// write header file with token defines
%defines

// use newer c++ skeleton files
%skeleton "lalr1.cc"

// namespace to enclose parser
%name-prefix="derivative"

// set parsers class identifier
%define "parser_class_name" "MathParser"

// keep track of the current position within the input
%locations
%initial-action
{
    // init initial location object
    @$.begin.filename = @$.end.filename = &driver.streamName;
};

/* The driver is passed by reference to the parser and to the scanner. This
 * provides a simple but effective pure interface, not relying on global
 * variables. */
%parse-param { class MathDriver &driver }

// verbose error messages
%error-verbose

/***** Begin Grammar Token Definitions *****/

%union {
    int             integerVal;
    double          doubleVal;
    std::string*    stringVal;
    class ExprNode* exprNode;
}

%token                  END     0 "end of file"
%token                  EOL     "end of line"
%token <integerVal>     INTEGER "integer"
%token <doubleVal>      DOUBLE  "double"
%token <stringVal>      STRING  "string"
%token                  DERIVATIVE
%token                  SINFUNC COSFUNC TANFUNC EXPFUNC SQRTFUNC
%token                  PLUS MINUS TIMES DIVIDE POWER
%token                  LPARENT RPARENT

%left PLUS MINUS
%left TIMES DIVIDE
%right POWER

%type <exprNode>        constant variable
%type <exprNode>        atomExpr powExpr unaryExpr mulExpr addExpr Expr

%destructor { delete $$; } STRING
%destructor { delete $$; } constant variable
%destructor { delete $$; } atomExpr powExpr unaryExpr mulExpr addExpr Expr

/***** End Grammar Token Definitions *****/

%{

#include "driver.h"
#include "scanner.h"

/* this "connects" the bison parser in the driver to the flex scanner class
 * object. it defines the yylex() function call to pull the next token from the
 * current lexer object of the driver context. */
#undef yylex
#define yylex driver.lexer->lex

%}

%% 
/***** Begin Grammar Rule Definitions *****/

constant:
        DOUBLE                      { $$ = new ConstantExpr($1); }
      | INTEGER                     { $$ = new ConstantExpr($1); }
;
variable:
        STRING                      {
                                        if(driver.context.isVariable(*$1)) {
                                            $$ = new VariableExpr(*$1);
                                        } else if(!driver.context.existsParameter(*$1)) {
                                            $$ = new ParameterExpr(*$1);
                                            //error(yyloc, std::string("Unknown variable \"") + *$1 + "\"");
                                            //delete $1;
                                            //YYERROR;
                                        } else {
                                            $$ = new ConstantExpr(driver.context.getParameter(*$1));
                                        }
                                    }
;
atomExpr:
        constant                    { $$ = $1; }
      | LPARENT Expr RPARENT        { $$ = $2; }
      | SINFUNC LPARENT Expr RPARENT   { $$ = new SinFunc($3); }
      | COSFUNC LPARENT Expr RPARENT   { $$ = new CosFunc($3); }
      | TANFUNC LPARENT Expr RPARENT   { $$ = new TanFunc($3); }
      | EXPFUNC LPARENT Expr RPARENT   { $$ = new ExpFunc($3); }
      | SQRTFUNC LPARENT Expr RPARENT  { $$ = new SqrtFunc($3); }
      | variable                    { $$ = $1; }
;
powExpr:
        atomExpr                    { $$ = $1; }
      | atomExpr POWER powExpr      { $$ = new PowerExpr($1, $3); }
      | atomExpr POWER MINUS powExpr    { $$ = new PowerExpr($1, new NegateExpr($4)); }
;
unaryExpr: 
        powExpr                     { $$ = $1; }
      | PLUS powExpr                { $$ = $2; }
      | MINUS powExpr               { $$ = new NegateExpr($2); }
;          
mulExpr:
        unaryExpr                   { $$ = $1; }
      | mulExpr TIMES unaryExpr     { $$ = new MultiplyExpr($1, $3); }
      | mulExpr DIVIDE unaryExpr    { $$ = new DivideExpr($1, $3); }
;
addExpr:
        mulExpr                     { $$ = $1; }
      | addExpr PLUS mulExpr        { $$ = new AddExpr($1, $3); }
      | addExpr MINUS mulExpr       { $$ = new SubtractExpr($1, $3); }
;
Expr:
      addExpr                       { $$ = $1; }
;
  assignment:
      DERIVATIVE STRING             { 
                                        driver.context.variable = *$2;
                                        std::cout << "Setting Variable :" << *$2 << std::endl;
                                        delete $2;
                                    }
      | STRING '=' constant         {
                                        driver.context.parameters[*$1] = $3->evaluate();
                                        std::cout << "Setting Parameter " << *$1
                                                  << " = " << driver.context.parameters[*$1] << "\n";
                                        delete $1;
                                        delete $3;
                                    }
;
//    | f(x) = Expr

start:
        /* empty */
      | start ';'
      | start EOL
      | start assignment ';'
      | start assignment EOL
      | start assignment END
      | start Expr ';'              { driver.context.expressions.push_back($2); }
      | start Expr EOL              { driver.context.expressions.push_back($2); }
      | start Expr END              { driver.context.expressions.push_back($2); }
;

/***** End Grammar Rule Definitions *****/

%%

/***** Additional Code *****/

void derivative::MathParser::error(const MathParser::location_type &l,
                                   const std::string &m)
{
    driver.error(l, m);
}