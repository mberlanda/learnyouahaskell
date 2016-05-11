# Syntax in Functions

#### Pattern matching
```bash
ghci> let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
ghci> [a+b | (a,b) <- xs]  
[4,7,6,8,11,4]  
```

#### Let it be
```bash
ghci> [let square x = x * x in (square 5, square 3, square 2)]  
[(25,9,4)]
ghci> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)  
(6000000,"Hey there!")
ghci> let zoot x y z = x * y + z  
ghci> zoot 3 9 2  
29  
ghci> let boot x y z = x * y + z in boot 3 4 2  
14  
ghci> boot  
<interactive>:1:0: Not in scope: `boot'
```
#### Case expressions
```hs
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
                   ...  
```