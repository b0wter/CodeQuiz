module Main where

import Parser
import Expr
import Show

import System.Exit (exitFailure, exitSuccess)

main = testExamples examples >> exitSuccess

testExamples (x:xs) = do
                        let (s, e) = x
                        putStrLn ("testing with " ++ s)
                        let result = testDerive s
                        if result /= e then error ("Expected: " ++ e ++ " Got: " ++ result ++ "\n") else testExamples xs
testExamples [] = return ()    

testDerive s = showE . eval . simplify . derive 'x' . testParser $ s

examples = [
            ("x^2+x^3-5", "2x+3x^2"),
            ("2x^10+2x^3", "20x^9+6x^2"),
            ("2x^10 +2x^3", "20x^9+6x^2"),
            ("2x^10 + 2x^3", "20x^9+6x^2"),
            ("2x^10+ x^3", "20x^9+3x^2"),
            ("2x^10 +2x ^3", "20x^9+6x^2"),
            ("2x^10+2x^3+10", "20x^9+6x^2"),
            ("2x^10+2x^3+sin(x)", "20x^9+6x^2-cos(x)"),
            ("2x^10(2x^3+10)", "20x^9*6x^2*(2x^3+10)"),
            ("2x^10(2x^3+10)", "20x^9*6x^2(2x^3+10)"),
            ("2x^10(2x^3+10)", "20x^9*6x^2/(2x^3+10)"),
            ("2x^-10", "-20x^-11")
           ]


