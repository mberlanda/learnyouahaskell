# Functionally Solving Problems

## Reverse Polish notation

([wiki](https://en.wikipedia.org/wiki/Reverse_Polish_notation))

>  for each token in the reversed postfix expression:
>  if token is an operator:
>    push token onto the operator stack
>    pending_operand ← False
>  else if token is an operand:
>    operand ← token
>    if pending_operand is True:
>      while the operand stack is not empty:
>        operand_1 ← pop from the operand stack
>        operator ← pop from the operator stack
>        operand ← evaluate operator with operand_1 and operand
>    push operand onto the operand stack
>    pending_operand ← True
> result ← pop from the operand stack

```
Prelude> :l 10_functionally_solving_problems/rpn_num.hs
*Main> solveRPN "2 3 +"
5
*Main> solveRPN "10 4 3 + 2 * -"
-4
*Main> solveRPN "90 3 -"
87
*Main> solveRPN "90 34 12 33 55 66 + * - + -"
4037
*Main> :l 10_functionally_solving_problems/rpn_float.hs
*Main> solveRPN "10 2 ^"
100.0
*Main> solveRPN "90 34 12 33 55 66 + * - + -"
4037.0
*Main> solveRPN "10 10 10 10 10 sum 4 /"
12.5
*Main> solveRPN "10 10 10 10 sum 4 /"
10.0
```

This is not really fault tolerant.
A better signature would be `solveRPN :: String -> Maybe Float`:
```hs
*Main> solveRPN "0 4 /"
0.0
*Main> solveRPN "4 0 /"
Infinity
*Main> solveRPN "foo bar /"
*** Exception: Prelude.read: no parse
*Main> solveRPN 3

<interactive>:14:10: error:
    • No instance for (Num String) arising from the literal ‘3’
    • In the first argument of ‘solveRPN’, namely ‘3’
      In the expression: solveRPN 3
      In an equation for ‘it’: it = solveRPN 3

```
