{-# LANGUAGE KindSignatures,
             TypeFamilies,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleContexts,
             FlexibleInstances #-}

-- Generalised sum types from datatypes á la carte
data (f :+: g) a = Inl (f a) | Inr (g a)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl v) = Inl $ fmap f v
    fmap f (Inr v) = Inr $ fmap f v

-- Fixpoints
data Fix f = Fix (f (Fix f))

-- catamorphism from \muf -> a
cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg (Fix f) = alg (fmap (cata alg) f)

-- subtyping for higher kinds
-- this can probably be generalised
class a :<: b where
    inf :: a -> b

instance a :<: a where
    inf = id

instance (f a) :<: ((f :+: g) a) where
    inf = Inl

instance ((f a) :<: (g a)) => (f a) :<: ((h :+: g) a) where
    inf = Inr . inf
