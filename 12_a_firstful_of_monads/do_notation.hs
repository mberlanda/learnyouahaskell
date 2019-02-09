marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just (x > 8)

justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x

fail :: (Monad m) => String -> m a
fail msg = error msg

wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x

listOfTuples :: [(Int,Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a','b']
    return (n,ch)
