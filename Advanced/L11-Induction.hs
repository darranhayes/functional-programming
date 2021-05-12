-- Induction on Numbers

data Nat = Zero | Succ Nat

{- Basic values for the above type using the 2 constructors:

Zero
Succ Zero
Succ (Succ Zero)
...

To the system, the above values have no inherent meaning.
-}

-- Infinitity can be represented by:
inf :: Nat
inf = Succ inf -- an infinite sequence ;)

{-
Principle of Induction

Prove for a Base case (e.g., Zero)
Prove for an Inductive case (e.g., from n to Succ n)
Proves for all n

(Base)        (Inductive)
P(Zero)       ∀n.[P(n) => P(Succ n)]
------------------------------------
             ∀n.P(n)
             (All)
-}

-- Example: given
add :: Nat -> Nat -> Nat
add Zero m     = m
add (Succ n) m = Succ (add n m)

{-
Show that "add n Zero = n", e.g.,

P(n) ☰ add n Zero = n

Base case
P(Zero) ☰ add Zero Zero = Zero -- apply add (the first equation for above)

Inductive case
P(n) => P(Succ n)

So need to prove:
add n Zero = n => add (Succ n) Zero = Succ n

"add n Zero" is the induction hypothesis

Strategy, start with the largest sub-term:
add (Succ n) Zero
  apply definition of add
= Succ (add n Zero)
  induction hypothesis
= Succ n
-}

{- Example: show that add is associative
add x (add y z) = add (add x y) z

To get started, try to correlate with the add definition itself:
x exists in the recursive cases twice
y exists in the recursive cases once
z does not exist in the recursive cases at all
Suggest, try solving for x

P(x) ☰ ∀y.∀z. add x (add y z) = add (add x y) z -- y z are quantified for all natural numbers

Base case: P(Zero)
Show: add Zero (add y z) = add (add Zero y) z
add Zero (add y z)
  work forwards, apply 1st definition of add function
  apply add
= add y z
  work backwards, and 'add zero to y', e.g., unapplying
  unapply add
= add (add Zero y) z

Inductive case: P(x) => P(Succ x), i.e.,
  add x (add y z) = add (add x y) z => add (Succ x) (add y z) = add (add (Succ x) y) z

Start with the most complicated terms, e.g., 'add (Succ x) (add y z)'

add (Succ x) (add y z)
  apply add (e.g., apply the second definition of add to effectively move the Succ constructor out)
= Succ (add x (add y z))
  apply induction hypothesis from above to re-bracket
= Succ (add (add x y) z)
= add (Succ (add x y)) z
= add (add (Succ x) y) z
-}

{- Induction on Lists

P([])     ∀x.∀xs.[P(xs) => P(x:xs)]
-----------------------------------
          ∀xs.P(xs)

e.g., proving a property for all lists, prove for the empty list, and prove for cons
-}

-- Example: given
rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

{-
Show that, reversing a reversed list, equals the original list, e.g., rev (rev xs) = xs

Base case:
  rev (rev [])
  apply rev
= rev []
  apply rev
= []

Inductive case:
  rev (rev xs) = xs => rev (rev (x:xs)) = x:xs

Start with rev (rev (x:xs))

rev (rev (x:xs))
  apply rev for non-empty list
= rev (rev xs ++ [x])
  need to apply some knowledge of ++, in this case, rev (xs ++ ys) = rev ys ++ rev xs. aka reverse distributes over the append function
= rev [x] ++ rev (rev xs)
  reversing a single item list does not change, rev [x] = [x]
= [x] ++ rev (rev xs)
  apply the induction hypothesis
= [x] ++ xs
  apply ++
= x:xs
-}