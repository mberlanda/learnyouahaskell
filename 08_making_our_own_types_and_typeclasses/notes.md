# Making Our Own Types and Typeclasses

#### Algebraic data types intro

```hs
data Bool = False | True  
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647  
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```

Examples:
```
ghci> surface (Rectangle (Point 0 0) (Point 100 100))  
10000.0  
ghci> surface (Circle (Point 0 0) 24)  
1809.5574  
ghci> nudge (Circle (Point 34 34) 10) 5 10  
Circle (Point 39.0 44.0) 10.0  
ghci> nudge (baseRect 40 100) 60 23  
Rectangle (Point 60.0 23.0) (Point 100.0 123.0) 
```

#### Record syntax
```
data Person = Person String String Int Float String String deriving (Show)
ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"  
ghci> guy  
Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"  

*Main> let p = Person{firstName="Tizio", lastName="Caio", age=28, height=1.90, phoneNumber="", flavor="x"}
*Main> p
Person {firstName = "Tizio", lastName = "Caio", age = 28, height = 1.9, phoneNumber = "", flavor = "x"}
*Main> age(p)
28
```

#### Type parameters
```
data Maybe a = Nothing | Just a
ghci> Just "Haha"  
Just "Haha"  
ghci> Just 84  
Just 84  
ghci> :t Just "Haha"  
Just "Haha" :: Maybe [Char]  
ghci> :t Just 84  
Just 84 :: (Num t) => Maybe t  
ghci> :t Nothing  
Nothing :: Maybe a  
ghci> Just 10 :: Maybe Double  
Just 10.0
```

#### Derived instances
```
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

# Eq, Ord
ghci> Saturday == Sunday  
False  
ghci> Saturday == Saturday  
True  
ghci> Saturday > Friday  
True  
ghci> Monday `compare` Wednesday  
LT  

# Show, Read
ghci> Wednesday  
Wednesday  
ghci> show Wednesday  
"Wednesday"  
ghci> read "Saturday" :: Day  
Saturday

# Bounded
ghci> minBound :: Day  
Monday  
ghci> maxBound :: Day  
Sunday

# Enum
ghci> succ Monday  
Tuesday  
ghci> pred Saturday  
Friday  
ghci> [Thursday .. Sunday]  
[Thursday,Friday,Saturday,Sunday]  
ghci> [minBound .. maxBound] :: [Day]  
[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]  
```