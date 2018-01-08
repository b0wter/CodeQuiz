findMaybes p xs = let res = [r | r@(Just x) <- xs, p xs] in
                    if null res then Nothing else head res

matchId i (A x) = i == (aId x)
matchId i (B x) = i == (bId x)
matchId i (C x) = i == (cId x)
matchId i (F x) = i == (fooId x)
matchId _     _ = False

getFrom cache i = findMaybes (matchId i) cache

cached en i cache = do
    case getFrom cache i of
     Nothing -> do
        n <- getEntity en i
        return (n, n:cache)
     n -> return (n, cache)

