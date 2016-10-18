{-# LANGUAGE KindSignatures,
             PolyKinds,
             TypeFamilies,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleContexts,
             FlexibleInstances,
             OverlappingInstances #-}

module DatatypesAlacarte where
import Control.Monad

-- Generalised sum types from datatypes á la carte
--
-- The problem here is that one can only represent
-- sums of kind * -> *. I don't know if it would be
-- possible to represent kinds like *, or ConstraintKind.
--
-- ConstraintKind is obviously representable in the
-- operational-alacarte package.
data (f :+ g) a = Inl (f a) | Inr (g a) deriving Show

infixr :+:

type family ((a :: k) :+: (b :: k)) :: k where
    ((a :+ as) :+: bs) = a :+ (as :+: bs)
    (a :+: bs) = a :+ bs

assocExample :: (Either Int :+ (Either Bool :+ Maybe)) ()
assocExample = inf ((Right ()) :: Either Bool ()) :: ((Either Int :+: Either Bool) :+: Maybe) ()

instance (Functor f, Functor g) => Functor (f :+ g) where
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

foldFree :: (Functor f) => (a -> b) -> (f b -> b) -> Free f a -> b
foldFree pure imp (Pure x)   = pure x
foldFree pure imp (Impure f) = imp (fmap (foldFree pure imp) f) 

class f :<: g where
    inf :: f a -> g a

instance f :<: f where
    inf = id

instance f :<: (f :+ g) where
    inf = Inl

instance (f :<: g) => f :<: (h :+ g) where
    inf = Inr . inf

inject :: (g :<: f) => g (Free f a) -> Free f a
inject = Impure . inf
