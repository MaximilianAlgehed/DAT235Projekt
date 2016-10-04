{-# LANGUAGE KindSignatures,
             TypeFamilies,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleContexts,
             FlexibleInstances #-}

-- subtyping for values
class a :<: b where
    inj :: a -> b

instance a :<: a where
    inj = id
-- Our domain specific languages
class Named (repr :: * -> *) where
    type Name repr

class (Named repr) => GraphDSL (repr :: * -> *) where
    type Edge repr
    type Node repr
    makeNode :: Name repr -> repr (Node repr)
    makeEdge :: Node repr -> Node repr -> Edge repr -> repr ()

class (Named repr) => ConstraintDSL (repr :: * -> *) where
    type Constraint repr
    type Domain repr
    type DVar repr
    makeDVar  :: Name repr -> repr (DVar repr)
    constrain :: repr (Constraint repr) -> repr ()
    lessThan  :: (a :<: Domain repr, b :<: Domain repr) => a -> b -> repr (Constraint repr)

data Sign = P | M

example :: (String ~ Name m,
            Int :<: Domain m,
            (DVar m) :<: Domain m,
            Sign ~ Edge m,
            GraphDSL m,
            ConstraintDSL m,
            Monad m) => m ()
example = do
            a <- makeNode "a"
            b <- makeNode "b"
            makeEdge a b P
            c <- makeDVar "c"
            constrain (c `lessThan` (5 :: Int))
