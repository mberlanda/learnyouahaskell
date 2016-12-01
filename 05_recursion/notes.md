# Syntax in Functions

#### Maximum awesome
```bash
Prelude> :t maximum
maximum :: Ord a => [a] -> a
```
#### Quick, sort!

> a sorted list is a list that has all the values smaller than (or equal to) the head of the list in front (and those values are sorted), then comes the head of the list in the middle and then come all the values that are bigger than the head (they're also sorted).

#### Thinking recursively

Pattern in recursion thinking:
- define an edge case: scenario where a recursive application doesn't make sense (e.g. list -> [], tree -> node without children)
- define a function: do something between some element and apply it to the rest

What to keep in mind for a recursive way to solve a problem:
- edge case
- identities
- whether you'll break apart the parameters of the function (e.g. lists broken into a head and a tail via pattern matching) and on which part you'll use the recursive call.