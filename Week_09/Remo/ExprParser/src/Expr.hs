module Expr (
    -- * Types
    Expr(..), Op(..), Number, Function, Sym,
    -- * Common operations
    add, sub, multiply, divide, power,
    -- * Working with expressions
    eval, simplify, derive, 
    -- * Constants
    knownSymbols, knownFunctions
    ) where

-- * Types 

-- | An arithmetic expression:
data Expr = Func Function Expr -- ^ Function expression
          | Derivative Sym Expr -- ^ Derivative expression
          | Opr Op Expr Expr -- ^ Binary operation using operator 'Op'
          | Neg Expr -- ^ Negation of an expression
          | Literal Number -- ^ Literal number
          | Symbol Sym -- ^ Literal symbol
          deriving (Show, Eq)

-- | Binary operators:
data Op = Pow | Mul | Div | Add | Sub deriving (Show, Eq)

-- | Numeric literals
type Number = Integer

-- | Function names
type Function = String 

-- | Symbols
type Sym = Char


-- * Constants

knownSymbols :: [Sym] -- ^ Allowed function symbols.
knownSymbols = "abcdfghklmnopqrstuvwxyz" 

knownFunctions :: [Function] -- ^ Supported mathematical functions.
knownFunctions = ["sin", 
                  "cos",
                  "tan",
                  "exp"]


-- * Shortcuts to construct common operations

-- | Add expressions.
add = Opr Add

-- | Subtract expressions.
sub = Opr Sub

-- | Multiply expressions
multiply = Opr Mul

-- | Divide expressions.
divide = Opr Div

-- | Raise an expression to the power of another
power = Opr Pow

-- | Square an expression.
square = flip power (Literal 2)


-- * Working with Expressions

--
-- | Evaluate an expression. Operations with literals get evaluated and reduced to their results.
--
eval :: Expr -> Expr
eval (Opr o (Literal a) (Literal b)) = Literal (runOp o a b)
    where runOp Add = (+)
          runOp Sub = (-)
          runOp Mul = (*)
          runOp Pow = (^)
          runOp Div = div
eval (Opr Mul a@(Literal _) b) = case b of 
                                  (Opr Mul c@(Literal _) d) -> multiply (eval $ multiply a c) d
                                  _ -> Opr Mul a (eval b)
eval (Opr o a b@(Literal _)) = Opr o (eval a) b
eval (Opr o a@(Literal _) b) = Opr o a (eval b)
eval e@(Opr o a b) = let result = Opr o (eval a) (eval b)
                      in if result == e then e else eval result
eval (Func f e) = Func f (eval e)
eval (Derivative x e) = derive x e
eval (Neg (Literal a)) = Literal . negate $ a
eval a = a

--
-- | Simplify an expresion. Common substitutions/reductions will be appplied. 
-- 
simplify :: Expr -> Expr

simplify (Opr Add a (Literal 0)) = simplify a
simplify (Opr Add (Literal 0) b) = simplify b
simplify (Opr Sub a (Literal 0)) = simplify a
simplify (Opr Sub (Literal 0) b) = simplify b

simplify (Opr Mul a (Literal 1)) = simplify a
simplify (Opr Mul (Literal 1) b) = simplify b
simplify (Opr Div a (Literal 1)) = simplify a

simplify (Opr Mul _ (Literal 0)) = Literal 0
simplify (Opr Mul (Literal 0) _) = Literal 0
simplify (Opr Div (Literal 0) _) = Literal 0

simplify (Opr Pow (Literal 1) _) = Literal 1
simplify (Opr Pow a (Literal 1)) = simplify a
simplify (Opr Pow (Literal 0) _) = Literal 0
simplify (Opr Pow _ (Literal 0)) = Literal 1

simplify e@(Opr o a b) = let result = Opr o (simplify a) (simplify b)
                          in if result == e then e else simplify result
simplify a = a

--
-- | Find the derivative of an expression for the given symbol. Simplification is neccessary afterwards.
--
derive :: Char -> Expr -> Expr
derive x (Symbol _) = Literal 1
derive x (Literal _) = Literal 0
derive x (Opr o f g) | o == Add = add f' g'
                     | o == Sub = sub f' g'
                     | o == Mul = add f'g fg'
                     | o == Div = divide (sub f'g fg') g_squared
--                     | o == Pow = multiply g . multiply f' $ power f (decrement g)
                     | o == Pow = multiply g . multiply f' $ power f (decrement g)
                  
    where f' = derive x f
          g' = derive x g
          f'g = multiply f' g
          fg' = multiply f g'
          decrement (Literal a) = Literal (a-1)
          decrement a = sub a (Literal 1)
          g_squared = square g
derive x y = error $ "no derivative known for " ++ show y 

