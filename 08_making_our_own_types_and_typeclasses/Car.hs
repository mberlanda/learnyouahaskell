-- file: 08_making_our_own_types_and_typeclasses/Car.hs
data Car a b c = Car { company :: a  
                     , model :: b  
                     , year :: c   
                     } deriving (Show)

tellCar :: (Show a) => Car String String a -> String  
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

tellCar' :: (Show a) => Car String String a -> String
tellCar' (Car c m y) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
