{-# LANGUAGE KindSignatures,
             PolyKinds,
             TypeFamilies,
             TypeOperators
             #-}

import qualified Data.ALaCarte as D

infixr :+:

type family ((a :: k) :+: (b :: k)) :: k where
    ((a D.:+: as) :+: bs) = a D.:+: (as :+: bs)
    (a :+: bs) = a D.:+: bs
