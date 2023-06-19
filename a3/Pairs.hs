
module Pairs where


import Prelude
import Terms
import Subst
import Beta
import YCombinator
import Booleans
import Church
import Factorial
import Fibonacci

-- your code here...

-- Pairs

-- The pair constructor
pair :: Term
pair = Lambda "x" (Lambda "y" (Lambda "f" (App (App (Var "f") (Var "x")) (Var "y"))))

-- The first projection
first :: Term -> Term
first p = App p Booleans.true

-- The second projection
second :: Term -> Term
second p = App p Booleans.false