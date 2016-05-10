# 02. Starting Out

#### An intro to lists
```bash
ghci> head [5,4,3,2,1]
ghci> tail [5,4,3,2,1]
ghci> last [5,4,3,2,1]
ghci> init [5,4,3,2,1]

ghci> lenght [5,4,3,2,1]  
ghci> null [5,4,3,2,1]
ghci> reverse [5,4,3,2,1]
ghci> take 4 [5,4,3,2,1]
ghci> drop 2 [5,4,3,2,1]

ghci> maximum [5,4,3,2,1]
ghci> minimum [5,4,3,2,1]
ghci> sum [5,4,3,2,1]
ghci> product [5,4,3,2,1]

ghci> 4 `elem` [5,4,3,2,1]
```

#### Texas ranges
```bash
ghci> [1..20]
ghci> ['K'..'Z']
ghci> take 10 (cycle [1,2,3])
ghci> take 10 (repeat 5)
ghci> replicate 3 10 
```

#### I'm a list comprehension
````bash
ghci> [x*2 | x <- [1..10]]  
[2,4,6,8,10,12,14,16,18,20]  
ghci> [x*2 | x <- [1..10], x*2 >= 12]  
[12,14,16,18,20]
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]  
[16,20,22,40,50,55,80,100,110]
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]  
[55,80,100,110]
ghci> let nouns = ["hobo","frog","pope"]  
ghci> let adjectives = ["lazy","grouchy","scheming"]  
ghci> [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]  
["lazy hobo","lazy frog","lazy pope","grouchy hobo","grouchy frog",  
"grouchy pope","scheming hobo","scheming frog","scheming pope"]
ghci> let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
ghci> [ [ x | x <- xs, even x ] | xs <- xxs]  
[[2,2,4],[2,4,6,8],[2,4,2,6,2,6]
````

#### Tuples
```bash
ghci> fst (8,11)  
8
ghci> snd (8,11)  
11
ghci> zip [1,2,3,4,5] [5,5,5,5,5]  
[(1,5),(2,5),(3,5),(4,5),(5,5)]

ghci> let triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
ghci> let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
```