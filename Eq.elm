module Eq where

{-|
Quasi-class representing types that can be compared for equality.
-}
type Eq a = {eq : a -> a -> Bool}

neq : Eq a -> (a -> a -> a)
neq inst = (\x y -> not <| inst.eq x y)
