{-# LANGUAGE KindSignatures,
             TypeFamilies,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleContexts,
             FlexibleInstances,
             OverlappingInstances #-}

module DatatypesAlacarte where
import Control.Monad

-- Generalised sum types from datatypes á la carte
data (f :+: g) a = Inl (f a) | Inr (g a)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl v) = Inl $ fmap f v
    fmap f (Inr v) = Inr $ fmap f v

-- Fixpoints
data Fix f = Fix (f (Fix f))

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg (Fix f) = alg (fmap (cata alg) f)

-- Free monad
data Free f a = Pure a | Impure (f (Free f a))

instance (Functor f) => Functor (Free f) where
    fmap f (Pure a) = Pure (f a)
    fmap f (Impure a) = Impure (fmap (fmap f) a)

instance (Functor f) => Applicative (Free f) where
    pure = return
    (<*>) = ap

instance (Functor f) => Monad (Free f) where
    return = Pure

    (Pure a) >>= f = f a
    (Impure a) >>= f = Impure $ fmap (>>=f) a

-- subtyping for higher kinds
-- this can probably be generalised
class f :<: g where
    inf :: f a -> g a

instance f :<: f where
    inf = id

instance f :<: (f :+: g) where
    inf = Inl

instance (f :<: g) => f :<: (h :+: g) where
    inf = Inr . inf

inject :: (g :<: f) => g (Free f a) -> Free f a
inject = Impure . inf