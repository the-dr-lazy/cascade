{-|
Module      : Cascade.Type.Monoid
Description : !!! INSERT MODULE SHORT DESCRIPTION !!!
Copyright   : (c) 2020-2021 Cascade
License     : MPL 2.0
Maintainer  : Mohammad Hasani <the-dr-lazy@pm.me> (the-dr-lazy.github.io)
Stability   : Stable
Portability : POSIX

!!! INSERT MODULE LONG DESCRIPTION !!!
-}

module Cascade.Type.Monoid (type (<>), Concat) where

infixr 6 <>
type a <> b = Concat a b
type family Concat (a :: k) (b :: k) :: k

type instance Concat ('[] :: [k]) (lst :: [k]) = lst
type instance Concat ((l ': ls) :: [k]) (lst :: [k]) = l ': Concat ls lst
