data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)

say :: (Show a) => a -> IO()
say f = putStrLn $ show $ f

main = do
        say $ fmap (++"ha") (CJust 0 "ho") -- CJust 1 "hoha"
        say $ fmap (++"he") (fmap (++"ha") (CJust 0 "ho")) -- CJust 2 "hohahe"
        say $ fmap (++"blah") CNothing -- CNothing
        say $ fmap id (CJust 0 "haha") -- CJust 1 "haha"
        say $ id (CJust 0 "haha") -- CJust 0 "haha"
        -- since it does not obey the functor laws, this fmap instance is not a Functor
