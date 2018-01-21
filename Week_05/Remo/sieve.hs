import Data.Time.Clock
import Data.List.Ordered (minus)

primesTo m = 2 : sieve [3,5..m]
    where
    sieve [] = []
    sieve (p:xs)
        | p*p > m   = p : xs 
        | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..m])

main = do
    start <- getCurrentTime
    let primes = primesTo 2000000
    putStrLn $ "Anzahl: " ++ (show .length) primes
    end <- getCurrentTime
    putStrLn $ "Zeit: " ++ (show . diffUTCTime end) start 
    putStrLn $ "Summe: " ++ (show . sum) primes
