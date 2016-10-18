{-# LANGUAGE KindSignatures,
             PolyKinds,
             TypeFamilies,
             TypeOperators,
             ConstraintKinds,
             GADTs
             #-}

import qualified Data.ALaCarte as D
import GHC.Prim

type family ((a :: k) :+: (b :: k)) :: k where
    ((a D.:+: as) :+: bs) = a D.:+: (as :+: bs)
    (a :+: bs) = a D.:+: bs

data M x fs a = M x (Maybe a)

e1 :: M Int () ()
e1 = M 0 Nothing
e2 :: M Bool () ()
e2 = M True Nothing
e3 :: M String () ()
e3 = M "Hi" Nothing

-- This compiles, so we know the type is actually:
-- () -> ((M Int) D.:+: (M Bool :+: M String)) () ()
assocExample :: () -> (((M Int) :+: (M Bool)) :+: (M String)) () ()
assocExample _ = D.Inl e1
assocExample _ = D.Inr (D.Inl e2)
assocExample _ = D.Inr (D.Inr e3)

infixr :+:

data ((f :: Constraint) :|: (g :: Constraint)) where
    L :: f => f :|: g
    R :: g => f :|: g
