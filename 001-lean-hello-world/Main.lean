import Lean4Tutorial
/- import data.nat -/

def main : IO Unit :=
  IO.println s!"Hello, {hello}!"

-- Define the property "is even" for natural numbers
/- def is_even (n : ℕ) : Prop := ∃ m, n = 2 * m -/

-- Check if 10 is even (results in `true`)
/- #check is_even 10 -/

-- Check if 11 is even (results in `false`)
/- #check is_even 11 -/

-- A theorem: For any number n, it's either even or the next number is even.
/- #check ∀ (n : ℕ), is_even n ∨ is_even (n + 1) -/


