# Data.List

> - `intersperse` takes an element and a list and then puts that element in between each pair of elements in the list.
> - `intercalate` takes a list of lists and a list. It then inserts that list in between all those lists and then flattens the result.
> - `transposes` a list of lists. If you look at a list of lists as a 2D matrix, the columns become the rows and vice versa.
> - `foldl'` and `foldl1'`
> - `concat` flattens a list of lists into just a list of elements. Doing `concatMap` is the same as first mapping a function to a list and then concatenating the list with concat.
> - `and` takes a list of boolean values and returns True only if all the values in the list are True.
> - `or` is like `and`, only it returns True if any of the boolean values in a list is True.
> - `any` and `all` take a predicate and then check if any or all the elements in a list satisfy the predicate, respectively. Usually we use these two functions instead of mapping over a list and then doing `and` or `or`.
> - `iterate` takes a function and a starting value. It applies the function to the starting value, then it applies that function to the result, then it applies the function to that result again, etc. It returns all the results in the form of an infinite list.
> - `splitAt` takes a number and a list. It then splits the list at that many elements, returning the resulting two lists in a tuple.
> - `takeWhile` takes elements from a list while the predicate holds and then when an element is encountered that doesn't satisfy the predicate, it's cut off
> - `dropWhile` is similar, only it drops all the elements while the predicate is true.
> - `span` is kind of like `takeWhile`, only it returns a pair of lists. The first list contains everything the resulting list from takeWhile would contain if it were called with the same predicate and the same list. The second list contains the part of the list that would have been dropped.

[and so on...](http://learnyouahaskell.com/modules#data-list)