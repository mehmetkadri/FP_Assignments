
{-# LANGUAGE GADTs #-} -- do not remove or modify this line

import Prelude

{- Positive Integers -}

data Pos where
    XI :: Pos -> Pos
    XO :: Pos -> Pos
    XH :: Pos

-- your code here...

{- Rational Numbers -}

data Rat where
    Frac :: Int -> Pos -> Rat

-- your code here...