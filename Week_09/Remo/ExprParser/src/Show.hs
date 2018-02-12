module Show (pprint, showE) where

import Expr

-- |Pretty print an 'Expr'ession
--
-- >>> putStr . pprint $ Opr Add (Opr Add (Literal 5) (Literal 6)) (Literal 7)
-- Opr Add
-- .Opr Add
-- ..Literal 5
-- ..Literal 6
-- .Literal 7
pprint :: Expr -> String
pprint e = pp 0 e
    where pp n (Opr o a b) = pad n $ 
                             "Opr " ++ show o ++ "\n" 
                             ++ pp (n+1) a 
                             ++ pp (n+1) b 
          pp n (Neg a) = pad n $ 
                         "Neg " ++ "\n"
                         ++ pp (n+1) a
          pp n (Func f a) = pad n $
                            "Func " ++ f ++ "\n"
                            ++ pp (n+1) a
          pp n (Derivative x a) = pad n $ 
                                  "Derivative" ++ show x ++ "\n"
                                  ++ pp (n+1) a

          pp n a = pad n $ 
                   show a ++ "\n"
          pad n = (replicate n '.' ++)

-- | Format expressions:
--
-- >>> showE (Opr Add (Opr Pow (Symbol 'x') (Literal 2)) (Symbol 'x'))
-- "x^2+x" 
--
showE (Literal c) = show c
showE (Neg e) = concat ["-", showE e]

showE (Symbol c) = [c] 
showE (Func f e) = f ++ showParens e

showE (Opr Add a b) = concat [showE a, "+", showE b]
showE (Opr Sub a b) = concat [showE a, "-", showE b]
showE (Opr Mul (Literal a) (Literal b)) = concat [show a, "*", show b]
showE (Opr Mul (Literal a) (Symbol x)) = concat [show a, [x]]
showE (Opr Mul (Literal a) (Opr Pow (Symbol x) (Literal e))) = concat [show a, [x], "^", show e]
showE (Opr Mul (Literal a) (Func f e)) = concat [show a, f, showParens e]
showE (Opr Mul (Literal a) b) = concat [show a, showParens b]
showE (Opr Mul b (Literal a)) = concat [show a, showParens b]
showE (Opr Mul a b) = concat [showParens a, "*", showParens b]
showE (Opr Div (Literal a) b) = concat [show a, showParens b]
showE (Opr Div b (Literal a)) = concat [show a, showParens b]
showE (Opr Div a b) = concat [showParens a, "/", showParens b]

showE (Opr Pow (Literal a) (Literal b)) = concat [show a, "^", show b]
showE (Opr Pow (Literal a) b) = concat [show a, "^", showParens b]

showE (Opr Pow (Symbol x) (Literal b)) = concat [[x], "^", show b]
showE (Opr Pow (Func f e) (Literal b)) = concat [f, "^", show b, showParens e]

showE (Opr Pow a (Literal b)) = concat [showParens a, "^", show b]
showE (Opr Pow a b) = concat [showParens a, "^", showParens b]

showE (Derivative x e) = concat ["d/d", [x], showParens e]

-- * Helper functions

-- | Helper function to format factors and coefficients.
-- Returns @a@ if it's not 1 or -1. 
showCoefficient   1  = ""
showCoefficient (-1) = "-"
showCoefficient   c  = show c

-- | Helper function returning given expression in parentheses.
showParens e = "(" ++ showE e ++ ")"

-- | Helper function to format powers. Returns exponent in parentheses if neccessary.
showPower b 1 = b
showPower b e | e < 0 = b ++ "^(" ++ show e ++ ")"
              | e >= 0 = b ++ "^" ++ show e


