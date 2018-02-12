import Parser
import Show

main = fmap (pprint . testParser) getLine >>= putStrLn
