
{-# LANGUAGE GADTs #-} -- do not remove or modify this line
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

import Prelude


-- Q1)
{- Positive Integers -}

data Pos where
    XI :: Pos -> Pos
    XO :: Pos -> Pos
    XH :: Pos

-- Q1.a)

pos2Int :: Pos -> Int
pos2Int XH = 1
pos2Int (XO p) = 2 * pos2Int p
pos2Int (XI p) = 2 * pos2Int p + 1

int2Pos :: Int -> Pos
int2Pos num
  | num < 2 = XH
  | even num = XO (int2Pos (num `div` 2))
  | otherwise = XI (int2Pos ((num - 1) `div` 2))

instance Show Pos where
    show p = show (pos2Int p)


-- Q1.b)

posEq :: Pos -> Pos -> Bool
posEq p1 p2 = pos2Int p1 == pos2Int p2

instance Eq Pos where
    p == q = posEq p q


-- Q1.c)

posLeq :: Pos -> Pos -> Bool
posLeq p1 p2 = pos2Int p1 <= pos2Int p2

instance Ord Pos where
    p <= q = posLeq p q


-- Q1.d)

posAdd :: Pos -> Pos -> Pos -- addition
posAdd p1 p2 = int2Pos (pos2Int p1 + pos2Int p2)

posMult :: Pos -> Pos -> Pos -- multiplication
posMult p1 p2 = int2Pos (pos2Int p1 * pos2Int p2)

posSubtr :: Pos -> Pos -> Pos -- subtraction
posSubtr p1 p2 = int2Pos (pos2Int p1 - pos2Int p2)

posSignum :: Pos -> Pos -- sign calculation
posSignum p = int2Pos (signum (pos2Int p))

posAbs :: Pos -> Pos -- absolute value calculation
posAbs p = int2Pos (abs (pos2Int p))

posFromInteger :: Integer -> Pos -- conversion from Integer
posFromInteger i = int2Pos (fromInteger i)

instance Num Pos where
    n + m = posAdd n m
    n * m = posMult n m
    abs n = posAbs n
    signum n = posSignum n
    fromInteger n = posFromInteger n
    n - m = posSubtr n m



-- Q2)
{- Rational Numbers -}

data Rat where
    Frac :: Int -> Pos -> Rat

-- Q2.a)

printRat :: Rat -> String
printRat r =
    case r of
        Frac n d -> show n ++ "/" ++ show d

instance Show Rat where
    show r = printRat r


-- Q2.b)

ratEq :: Rat -> Rat -> Bool
ratEq (Frac n1 d1) (Frac n2 d2) = (n1 * (pos2Int d2)) == (n2 * (pos2Int d1))

instance Eq Rat where
    p == q = ratEq p q


-- Q2.c)

ratLeq :: Rat -> Rat -> Bool
ratLeq (Frac n1 d1) (Frac n2 d2) = (n1 * (pos2Int d2)) <= (n2 * (pos2Int d1))

instance Ord Rat where
    p <= q = ratLeq p q


-- Q2.d)

ratAdd :: Rat -> Rat -> Rat -- addition
ratAdd (Frac n1 d1) (Frac n2 d2) = 
    Frac ((n1 * (pos2Int d2)) + (n2 * (pos2Int d1))) (int2Pos((pos2Int d1) * (pos2Int d2)))

ratMult :: Rat -> Rat -> Rat -- multiplication
ratMult (Frac n1 d1) (Frac n2 d2) = 
    Frac (n1 * n2) (int2Pos ((pos2Int d1)*(pos2Int d2)))

ratSubtr :: Rat -> Rat -> Rat -- subtraction
ratSubtr (Frac n1 d1) (Frac n2 d2) = 
    Frac ((n1 * (pos2Int d2)) - (n2 * (pos2Int d1))) (int2Pos((pos2Int d1) * (pos2Int d2)))

ratSignum :: Rat -> Rat -- sign calculation
ratSignum (Frac n d) = Frac (signum n) (int2Pos 1)

ratAbs :: Rat -> Rat -- absolute value calculation
ratAbs f = ratMult (ratSignum f) f

ratFromInteger :: Integer -> Rat -- conversion from Integer
ratFromInteger i = Frac (fromInteger i) XH

instance Num Rat where
    n + m = ratAdd n m
    n * m = ratMult n m
    n - m = ratSubtr n m
    signum n = ratSignum n
    abs n = ratAbs n
    fromInteger n = ratFromInteger n