-- file: 08_making_our_own_types_and_typeclasses/TrafficLight.hs

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red    == Red    = True
  Yellow == Yellow = True
  Green  == Green  = True
  _ == _ = False

instance Show TrafficLight where  
  show Red = "Red light"  
  show Yellow = "Yellow light"  
  show Green = "Green light"