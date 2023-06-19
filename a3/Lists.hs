
module Lists where

import Prelude
import Terms
import Subst
import Beta
import YCombinator
import Booleans
import Church
import Factorial
import Fibonacci
import Pairs

-- your code here...

-- Lists

-- The empty list
nil :: Term
nil = Lambda "l" (Var "l")

-- The list constructor
-- cons := λx.λl.pair false (pair x l)
cons :: Term -> Term -> Term
cons x l = App (App Pairs.pair Booleans.false) (App (App Pairs.pair x) l)


-- The head of a list
-- λl.first (second l)
hd :: Term -> Term
hd l = Pairs.first (Pairs.second l)

-- The tail of a list
tl :: Term -> Term
tl l = Pairs.second (Pairs.second l)

-- The null predicate
isNull :: Term -> Term
isNull l = Pairs.first l


-- The length of a list
-- λƒ .λl.ite (isNull l) (zero) (addition (one) (ƒ (tl l)))
leng :: Term
leng = 
    Lambda "ƒ"
    (
        Lambda "l"
        (
            ite
            (isNull (Var "l"))
            (Church.zero)
            (addition (Church.one) (App (Var "ƒ") (tl (Var "l"))))
        )
    )

length :: Term -> Term
length l = App(App(yCombinator)(leng))(l)


-- Append function
-- append := λƒ .λl1.λl2ite (isNull l1) (l2) (cons (hd l1) (ƒ (tl l1) l2))

app :: Term
app = 
    Lambda "ƒ"
    (
        Lambda "l1"
        (
            Lambda "l2"
            (
                ite
                (isNull (Var "l1"))
                (Var "l2")
                (cons (hd (Var "l1")) (App (App (Var "ƒ") (tl (Var "l1"))) (Var "l2")))
            )
        )
    )

append :: Term -> Term -> Term
append l1 l2 = App (App (App (yCombinator) (app)) (l1)) (l2)

-- The reverse function
-- reverse := λƒ .λl1.λl2.ite (isNull l1) (l2) (ƒ (tl l1) (cons (hd l1) (l2)))
rev :: Term
rev = 
    Lambda "ƒ"
    (
        Lambda "l1"
        (
            Lambda "l2"
            (
                ite
                (isNull (Var "l1"))
                (Var "l2")
                (App (App (Var "ƒ") (tl (Var "l1"))) (cons (hd (Var "l1")) (Var "l2")))
            )
        )
    )

reverse :: Term -> Term
reverse l = App (App (App (yCombinator) (rev)) (l)  ) nil
