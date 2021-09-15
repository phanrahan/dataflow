Implementation of reactive programming using Streams in Haskell. 

This depends on three essential features of Haskell.

1. Infinite lists which is possible because of lazy evaluation
2. Non-strict evaluation - only some arguments to a function are evaluated.
3. Recursive expressions - again due to lazy evaluation

% ghci stream.hs
ghci> head tff
[False]
ghci> take 2 tff
[False, True]
ghci> take 4 lo
[False, False, False, False]
