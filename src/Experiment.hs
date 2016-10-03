{-# LANGUAGE KindSignatures,
             TypeFamilies,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleContexts,
             FlexibleInstances #-}

data Sign = P | M

newtype Node = Node Int

newtype DVar = DVar Int

class a :<: b where
    inj :: a -> b

instance a :<: a where
    inj = id

class Named (repr :: * -> *) where
    type Name repr

class (Named repr) => GraphDSL (repr :: * -> *) where
    makeNode :: Name repr -> repr Node
    makeEdge :: Node -> Node -> Sign -> repr ()

class (Named repr) => ConstraintDSL (repr :: * -> *) where
    type Constraint repr
    type Domain repr
    makeDVar  :: Name repr -> repr DVar
    constrain :: repr (Constraint repr) -> repr ()
    lessThan  :: (a :<: Domain repr, b :<: Domain repr) => a -> b -> repr (Constraint repr)

example :: (String ~ Name m,
            Int :<: Domain m,
            DVar :<: Domain m,
            GraphDSL m,
            ConstraintDSL m,
            Monad m) => m ()
example = do
            a <- makeNode "a"
            b <- makeNode "b"
            makeEdge a b P
            c <- makeDVar "c"
            constrain (c `lessThan` (5 :: Int))
