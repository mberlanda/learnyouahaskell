-- file: 08_making_our_own_types_and_typeclasses/Person.hs

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)