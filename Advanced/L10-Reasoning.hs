{-
# Equational Reasoning


commutativity (order), e.g., multiplication
(1) x y = y x

associativity (grouping/associating), e.g., addition
(2) x + (y + z) = (x + y) + z

distributivity
(3) x (y + z) = x y + x z -- left distributivity
(4) (x + y) z = x z + y z -- right distributivity

Prove this equation using the above:

(x + a) (x + b) = x^2 + (a + b) x + a b

  (x + a) (x + b)
= (x + a) x + (x + a) b -- used (3) where x=y & z=b
= (x x + a x) + (x b + a b) -- used (4)
= x x + a x + x b + a b -- removed brackets using (2) associativity
= x^2 + a x + x b + a b -- simplify
= x^2 + a x + b x + a b -- used (1) commutativity (switch x b)
= x^2 + (a + b) x + a b -- used (4) right distributivity
= proved

# Reasoning about Haskell

double :: Int -> Int
double x = x + x

Functions can be "applied from left to right" (sub 'double 10' for '10 + 10')
Functions can be "unapplied from right to left" (sub '5 + 5' for 'double 5')

However, watch out for overlapping patterns:
isZero :: Int -> Bool
isZero 0 = True -- only this case can be used in both apply and unapply cases
isZero n = False -- if unapplying here, can end up with invalid proofs

Use, disjoint or non-overlapping patterns:
isZero :: Int -> Bool
isZero 0 = True
isZero n | n /= 0 = False

Note: Most Haskell library functions are defined with disjoint patterns!
-}