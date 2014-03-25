module Ord where

import Eq (..)

type Ord a = { eqInst: Eq a,
               compare: a -> a -> Order,
               lt : a -> a -> Bool,
               leq : a -> a -> Bool,
               gt : a -> a -> Bool,
               geq : a -> a -> Bool,
               min : a -> a -> a,
               max : a -> a -> a
            }
            
minimalOrd : Eq a -> (a -> a -> Bool) -> Ord a
minimalOrd eqInst lt = {eqInst = eqInst,
                        lt = lt,
                        compare = (\x y -> if eqInst.eq x y
                                           then EQ
                                           else if lt x y
                                           then LT
                                           else GT),
                        leq = (\a b -> lt a b || eqInst.eq a b)
                        gt = (\a b -> not <| (lt a b || eqInst.eq a b)),
                        geq = (\ a b -> not <| lt a b),
                        min = (\a b -> if (lt a b) then a else b),
                        max = (\a b -> if (lt a b) then b else a)
                        }
