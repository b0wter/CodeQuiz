module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Expr

-- Language definition for Parsec.
languageDef = emptyDef {
                Token.reservedOpNames = ["^", "*", "/", "+", "-", "d/d"],
                Token.reservedNames = knownFunctions ++ map (:"") knownSymbols
              }
lexer = Token.makeTokenParser languageDef

operator = Token.reservedOp lexer
parens = Token.parens lexer
reserved = Token.reserved lexer
symbol = Token.symbol lexer
--number = Token.naturalOrFloat lexer
number = Token.integer lexer
whitespace = Token.whiteSpace lexer


-- Parse a literal number.
exprLiteral :: Parser Expr
exprLiteral = Literal <$> number

-- Parse a power operator and its right argument. Used by 'exprSymbol' and 'exprFunc'.
exprPower = operator "^" >> exprLiteral

-- Parse a symbol, optionally with factor and exponent.
exprSymbol :: Parser Expr
exprSymbol = do 
              n <- option (Literal 1) exprLiteral
              c <- choice (map char knownSymbols)
              e <- option (Literal 1) exprPower
              let symb = Symbol c
              if e == Literal 1 
                then return (factor n symb)
                else return (factor n . power symb $ e)

-- Parse a function, optionally with factor and exponent.
exprFunc = do 
            n <- option (Literal 1) exprLiteral
            f <- choice (map string knownFunctions)
            e <- option (Literal 1) exprPower
            t <- parens expr
            let fun = Func f t
            if e == Literal 1 
             then return (factor n fun)
             else return (factor n . power fun $ e)

-- Helper for factorized terms:
factor (Literal 1) f = f
factor n f = multiply n f

-- All possible terms.
term =  parens (expr)
    <|> try (exprFunc)
    <|> try (exprSymbol)
    <|> (Literal <$> number)

-- Complete expression parser.
expr = buildExpressionParser operators term
operators = [ [Prefix (operator "-" >> return (Neg)) ,
               Prefix (operator "d/d" >> oneOf knownSymbols >>= return . Derivative)],
              [Infix (operator "^" >> return power) AssocRight,
               Infix (operator "*" >> return multiply) AssocLeft,
               Infix (operator "/" >> return divide) AssocLeft],
              [Infix (operator "-" >> return sub) AssocLeft,
               Infix (operator "+" >> return add) AssocLeft]
            ]

-- Parse all text.
parseAll = whitespace >> expr >>= (\e -> eof >> return e)

-- Parse input to expression tree or fail with parse error message.
testParser s = case parse (parseAll) "test" (filter (/=' ') s) of
                  Right r -> r
                  Left m -> error $ show m
                  
