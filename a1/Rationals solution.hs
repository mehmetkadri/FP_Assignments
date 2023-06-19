

{-# LANGUAGE GADTs #-}

import Prelude

data Pos where
    XI :: Pos -> Pos
    XO :: Pos -> Pos
    XH :: Pos

pos2Int :: Pos -> Int
pos2Int p =
    case p of
        XI k -> 2 * pos2Int k + 1
        XO k -> 2 * pos2Int k
        XH   -> 1

int2Bin :: Int -> [Bool] -> [Bool]
int2Bin i l =
    if i <= 1 then l
    else if mod i 2 == 0 then int2Bin (div i 2) (False : l)
    else  int2Bin (div i 2) (True : l)

int2Pos :: Int -> Pos 
int2Pos i =
    if i < 1 then error "input a non-negative integer"
    else
        let l = (int2Bin i [])
        in traverse l XH
        where
            traverse l p =
                case l of
                    []   -> p
                    x:xs -> if x == True then traverse xs (XI p) else traverse xs (XO p)

printPos :: Pos -> String
printPos p =
    case p of
        XH   -> "XH"
        XI k -> "XI (" ++ printPos k  ++ ")"
        XO k -> "XO (" ++ printPos k  ++ ")"

instance Show Pos where
    show p = show (pos2Int p)

instance Eq Pos where
    p == q = (pos2Int p == pos2Int q)

instance Ord Pos where
    p <= q = (pos2Int p <= pos2Int q)

posAdd :: Pos -> Pos -> Pos
posAdd p q =
    int2Pos (pos2Int p + pos2Int q)

posMult :: Pos -> Pos -> Pos
posMult p q =
    int2Pos (pos2Int p * pos2Int q)

posSubtr :: Pos -> Pos -> Pos
posSubtr p q =
    int2Pos (pos2Int p - pos2Int q)

posAbs :: Pos -> Pos
posAbs p = p

posSignum :: Pos -> Pos
posSignum p = XH

posFromInteger :: Integer -> Pos
posFromInteger i =
    if i <= 0 then error "positives are non-negative integers"
    else (int2Pos (fromInteger i))

instance Num Pos where
    n + m = posAdd n m
    n * m = posMult n m
    abs n = posAbs n
    signum n = posSignum n
    fromInteger n = posFromInteger n
    n - m = posSubtr n m

instance Read Pos where
    readsPrec _ l = 
        let i = (read l :: Int) in [(int2Pos i,"")]

{- Rational Numbers -}

data Rat where
    Frac :: Int -> Pos -> Rat

instance Show Rat where
    show r = 
        case r of
            Frac i p -> show i ++ "/" ++ show p

ratEq :: Rat -> Rat -> Bool
ratEq p q =
    case p of
        Frac a b ->
            case q of
                Frac c d ->
                    if a * (pos2Int d) == c * (pos2Int b) then True else False

instance Eq Rat where
    p == q = ratEq p q

ratLeq :: Rat -> Rat -> Bool
ratLeq p q =
    case p of
        Frac a b ->
            case q of 
                Frac c d ->
                    if (pos2Int b) == (pos2Int d) then a <= c
                    else 
                        let ua = a * (lcm (pos2Int b) (pos2Int d) `div` (pos2Int b))
                            uc = c * (lcm (pos2Int b) (pos2Int d) `div` (pos2Int d))
                        in ua <= uc

instance Ord Rat where
    p <= q =
        ratLeq p q

ratAdd :: Rat -> Rat -> Rat
ratAdd p q =
    case p of
        Frac a b ->
            case q of
                Frac c d ->
                    Frac ((((lcm (pos2Int b) (pos2Int d)) `div` (pos2Int b)) * a) + (((lcm (pos2Int b) (pos2Int d)) `div` (pos2Int d)) * c)) 
                         (int2Pos (lcm (pos2Int b) (pos2Int d)))

ratSubtr :: Rat -> Rat -> Rat
ratSubtr p q =
    case p of
        Frac a b ->
            case q of
                Frac c d ->
                    Frac ((((lcm (pos2Int b) (pos2Int d)) `div` (pos2Int b)) * a) - (((lcm (pos2Int b) (pos2Int d)) `div` (pos2Int d)) * c)) 
                         (int2Pos (lcm (pos2Int b) (pos2Int d)))

ratMult :: Rat -> Rat -> Rat
ratMult p q =
    case p of
        Frac a b ->
            case q of 
                Frac c d -> Frac (a*c) (int2Pos (pos2Int b * pos2Int d))

ratAbs :: Rat -> Rat
ratAbs p =
    case p of
        q@(Frac a b) -> if a < 0 then Frac (-a) b else q

ratSignum :: Rat -> Rat
ratSignum p =
    case p of
        Frac a b -> if a > 0 then Frac 1 XH else if a == 0 then Frac 0 XH else Frac (-1) XH

ratFromInteger :: Integer -> Rat
ratFromInteger i =
    Frac (fromInteger i) XH

instance Num Rat where
    n + m = ratAdd n m
    n * m = ratMult n m
    abs n = ratAbs n
    signum n = ratSignum n
    fromInteger n = ratFromInteger n
    n - m = ratSubtr n m

dropSlash :: String -> String
dropSlash s = 
    case s of
        []   -> []
        x:xs -> if (x == ' ' || x == '/') then dropSlash xs else x: dropSlash  xs  

instance Read Rat where
    readsPrec _ l = let (i, r) = span (/= '/') l
                        rest      = dropSlash r
                    in
                        let n = (read i :: Int) 
                            d = (read rest :: Pos)
                        in
                            [(Frac n d, "")]

