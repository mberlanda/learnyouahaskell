type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left,right) = (left + n,right)

landRight :: Birds -> Pole -> Pole
landRight n (left,right) = (left,right + n)

(-:) :: a -> (a -> b) -> b
x -: f = f x

mlandLeft :: Birds -> Pole -> Maybe Pole
mlandLeft n (left,right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

mlandRight :: Birds -> Pole -> Maybe Pole
mlandRight n (left,right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing
 {-
(>>) :: (Monad m) => m a -> m b -> m b
m >> n = m >>= \_ -> n
 -}

routine :: Maybe Pole
routine = case mlandLeft 1 (0,0) of
    Nothing -> Nothing
    Just pole1 -> case mlandRight 4 pole1 of
        Nothing -> Nothing
        Just pole2 -> case mlandLeft 2 pole2 of
            Nothing -> Nothing
            Just pole3 -> mlandLeft 1 pole3

mroutine :: Maybe Pole
mroutine = return (0,0) >>= mlandLeft 1 >>= mlandRight 4 >>= mlandLeft 2 >>= mlandLeft 1

-- do notation
doroutine :: Maybe Pole
doroutine = do
    start <- return (0,0)
    first <- mlandLeft 2 start
    second <- mlandRight 2 first
    mlandLeft 1 second

nroutine :: Maybe Pole
nroutine =
    case Just (0,0) of
        Nothing -> Nothing
        Just start -> case mlandLeft 2 start of
            Nothing -> Nothing
            Just first -> case mlandRight 2 first of
                Nothing -> Nothing
                Just second -> mlandLeft 1 second


ndoroutine :: Maybe Pole
ndoroutine = do
    start <- return (0,0)
    first <- mlandLeft 2 start
    Nothing
    second <- mlandRight 2 first
    mlandLeft 1 second
