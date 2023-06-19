
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module DFA where

import Prelude
import Data.Set
import Data.Set.Internal

type State = Int

data DFA =
    DFA
    {
        dfa_state    :: Set State,
        dfa_alphabet :: Set Char,
        dfa_sigma    :: State -> Char -> State,
        dfa_start    :: State,
        dfa_finals   :: Set State
    }

-- Q1: 

---- a) multi-step transition function

dfa_multiStep :: DFA -> State -> String -> State
dfa_multiStep dfa state s =
    case s of
        [] -> state
        (x:xs) -> dfa_multiStep dfa (dfa_sigma dfa state x) xs 


---- b) acceptance criterion

dfa_acceptance :: DFA -> String -> Bool
dfa_acceptance dfa string =
    member (dfa_multiStep dfa (dfa_start dfa) string) (dfa_finals dfa)



-- Q2: DFA M1 :={0,1,2,3,4,5},{a,b},δ,0,{5}

---- a) 

dfa_m1_sigma :: State -> Char -> Set Char -> State
dfa_m1_sigma state char dfa_alphabet=
    if member char dfa_alphabet then
        case state of
            0 -> if char == 'a' then 1 else 0
            1 -> if char == 'a' then 2 else 0
            2 -> if char == 'a' then 2 else 3
            3 -> if char == 'a' then 4 else 0
            4 -> if char == 'a' then 2 else 5
            5 -> 5
    else error "invalid input"

dfa_m1 :: DFA
dfa_m1 =
    DFA
    {
        dfa_state = fromList [0,1,2,3,4,5],
        dfa_alphabet = fromList ['a','b'],
        dfa_sigma = \state char -> dfa_m1_sigma state char (dfa_alphabet dfa_m1),
        dfa_start = 0,
        dfa_finals = fromList [5]
    }

---- b) 

-- function that takes an input
dfa_m1_acceptance :: String -> Bool
dfa_m1_acceptance = dfa_acceptance dfa_m1

-- examples:
-- baaababa     : accepted
-- bbbabaaaba   : rejected


-- Q3: DFA M2 :={0,1,2,3,4,5,6,7,8,9,10},{a},δ,0,{0,2,4,6,7,8,10,12}

---- a) 

dfa_m2_sigma :: State -> Char -> Set Char -> State
dfa_m2_sigma state char dfa_alphabet=
    if member char dfa_alphabet then
        case state of
            13 -> 0
            _ -> state + 1
    else error "invalid input"

dfa_m2 :: DFA
dfa_m2 =
    DFA
    {
        dfa_state = fromList [0,1,2,3,4,5,6,7,8,9,10,11,12,13],
        dfa_alphabet = fromList ['a'],
        dfa_sigma = \state char -> dfa_m2_sigma state char (dfa_alphabet dfa_m2),
        dfa_start = 0,
        dfa_finals = fromList [0,2,4,6,7,8,10]
    }


---- b) 

-- function that takes an input
dfa_m2_acceptance :: String -> Bool
dfa_m2_acceptance = dfa_acceptance dfa_m2

-- examples:
-- aaaaaaa     : accepted
-- aaaaa   : rejected

-- functions that take the input beforehand
dfa_m2_acceptance_1 :: Bool
dfa_m2_acceptance_1 = dfa_acceptance dfa_m2 "aaaaaaa"

dfa_m2_acceptance_2 :: Bool
dfa_m2_acceptance_2 = dfa_acceptance dfa_m2 "aaaaa"