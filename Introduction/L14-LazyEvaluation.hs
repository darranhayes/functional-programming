{--
# Lazy evaluation:
Avoids *unnecessary* evaluation;
Ensures *termination* whenever possible;
Supports programming with *infinite* lists;
Allows programms to be more *modular*;

## Evaluating Expressions: "Apply definitions until no further simplification is possible"

square n = n * n

_ = square (1+2)

Simplify starting with (+) first:
square (1+2)
square 3
3 * 3
9

Simplify with square first:
square (1+2)
(1+2) * (1+2)
3 * (1+2)
3 * 3
9

"Any way of evaluating the *same* expression will give the *same* result, provided it terminates"

Two approaches for deciding which Reducible Expression (redex) to consider next:
- Innermost (it does not contain another redex - it's as small as possible)
- Outermost (it is not contained in another redex - it's as big as possible)

## Termination
infinity = 1 + infinity

_ = fst (0, infinity)

Innermost:
expand 'infinity' as it's the innermost expression that remains to be evaluated
fst (0, infinity)
fst (0, 1 + infinity)
fst (0, 1 + (1 + infinity))
fst (0, 1 + (1 + (1 + infinity)))
...
Using Innermost evaluation, this expression will not terminate.

Outermost:
expand 'fst' first, as it's the outermost expression and not contained within another expression
fst (0, infinity)
0 fst returns the first item in the tuple
Using Outermost evaluation, may give a result when innermost fails to terminate

"If *any* evaluation sequence terminates, then so does outermost, with the same result"

# Number of reductions

Innermost square (1+2)
square 3
3 * 3
9
3 steps

Outermost square (1+2)
(1+2) * (1+2)
3 * (1+2)
3 * 3
9
4 steps

Outermost version is *inefficient*, because the argument 1+2 is duplicated when square is applied. 1+2 is evaluated twice.
Due to duplication, outermost evaluation may require *more* steps than innermost.
Problem can be avoided by using *pointers* to indicate sharing of arguments.

Outermost square (1+2) with shared pointers to arguments, aka: (1+2)
{1+2} * {1+2}
3 * 3 evaluate {1+2} once and share the result on both sides of *
9
3 steps

*Lazy Evaluation = Outermost evaluation + sharing of arguments*

Lazy Evaluation ensures *termination* whenever possible, but *never* requires more steps than innermost, and sometimes fewer.

## Infinite Lists

ones = 1 : ones

ones
1 : ones
1 : (1 : ones)
1 : (1 : (1 : ones))
1 : (1 : (1 : (1 : ones)))
...

_ = head ones
Innermost
head ones ones is the innermost expression to be evaluated
head (1 : ones) ones is the innermost expression to be evaluated
head (1 : (1 : ones)) ones is the innermost expression to be evaluated
...
Does not terminate

Lazy / or rather Outermost as there's no argument sharing ;)
head ones need to expand definition of ones as head has nothing to work with yet
head (1 : ones) after one expansion of ones, *now* head can be evaluated
1
Terminates in 2 steps

In Lazy case, only the *first* element of ones is produced, as the rest are not required.
In general, with Lazy, expressions are only evaluated as *much as required* by the context in which they are used.
Hence, ones is really a *potentially* infinite list.

## Modular Programming

Lazy evaluation allows us to make programs more *modular* by separating *control* from *data*.

_ = take 5 ones
> [1,1,1,1,1]

The data part "ones" is only evaluated as much as required by the control part "take 5".

Without using lazy evaluation, the control and data parts need to be *combined* into one function:

Naive implemention of replicate using primative recursion:

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x -- control and data aspects combined

vs

repeat x = x : repeat x -- data part producing infinite list
replicate n x = take n (repeat x) -- control part: take n, separated from data part (repeat)

## Generating infinite sequences of primes - Sieve of Eratosthenes

1. Infinite sequence of numbers from 2; (generate *infinite sequence)
2. Mark first number p as prime;
3. Delete all multiples of p from the sequence; (transform *infinite sequence)
4. Go to 2. (repeat *infinite number of times)
* in total, 3 infinite sequences

2 3 4 5 6 7 8 9 10 11 12 13 14 15
p   x   x   x   x     x     x
  p           x                x
      p         x
          p
                   p
                         p
2 3 5 7 11 13
--}

primes = sieve [2..]

sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]

_ = takeWhile (<20) primes -- primes is data part, takeWhile is control part

twin (x,y) = y == x+2

twinPrimes = [ (x, y) | (x, y) <- zip primes (drop 1 primes), twin (x, y) ]
-- or
twinPrimes' = filter twin (zip primes (tail primes))