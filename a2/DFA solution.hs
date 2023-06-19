
{-# LANGUAGE GADTs #-}

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
        dfa_delta    :: State -> Char -> State,
        dfa_start    :: State,
        dfa_finals   :: Set State
    }

data DFAA where
    DFAA :: Set State -> Set Char -> (State -> Char -> State) -> State -> Set State -> DFAA


dfa_multiStep :: DFA -> State -> String -> State
dfa_multiStep d q s =
    if member q (dfa_state d) 
    then chain d q s
    else error "not a valid state"
    where 
        chain d q s =
            case s of
                []   -> q
                x:xs -> chain d ((dfa_delta d) q x) xs

dfa_acceptance :: DFA -> String -> Bool
dfa_acceptance d x =
    let q = dfa_multiStep d (dfa_start d) x
    in member q (dfa_finals d)
    
dfa1 :: DFA
dfa1 = 
    DFA (fromList [0..5]) (fromList ['a', 'b']) (delta_dfa1) 0 (fromList [5])
    where
        delta_dfa1 s c =
            case (s, c) of
                (0, 'a') -> 1
                (0, 'b') -> 0
                (1, 'a') -> 2
                (1, 'b') -> 0
                (2, 'a') -> 2
                (2, 'b') -> 3
                (3, 'a') -> 4
                (3, 'b') -> 0
                (4, 'a') -> 2
                (4, 'b') -> 5
                (5, 'a') -> 5
                (5, 'b') -> 5

dfa2 :: DFA
dfa2 = 
    DFA (fromList [0..13]) (fromList ['a']) delta_dfa2 0 (fromList[0,2,4,6,7,8,10,12])
    where
        delta_dfa2 s c =
            case (s, c) of
                (0, 'a') -> 1
                (1, 'a') -> 2
                (2, 'a') -> 3
                (3, 'a') -> 4
                (4, 'a') -> 5
                (5, 'a') -> 6
                (6, 'a') -> 7
                (7, 'a') -> 8
                (8, 'a') -> 9
                (9, 'a') -> 10
                (10, 'a') -> 11
                (11, 'a') -> 12
                (12, 'a') -> 13
                (13, 'a') -> 0




