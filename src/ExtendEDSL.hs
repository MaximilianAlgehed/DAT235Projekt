{-# LANGUAGE GADTs, TypeOperators, FlexibleContexts #-}
import DatatypesAlacarte
import EDSL_Compilation

-- These denote our "High" group expressions
-- We will also need some "low" group expressions
-- in the target monad (like the Run monad)
data Group where
    Get          :: Int -> Group
    Union        :: Group -> Group -> Group
    Intersection :: Group -> Group -> Group

reifyGroup :: Group -> Prog HighExp Int
reifyGroup (Get i)       = return i
reifyGroup (Union gl gr) = do
                             l <- reifyGroup gl
                             r <- reifyGroup gr
                             printStr $ "taking union "++(show l)++", "++(show r)
                             return (l+r)
reifyGroup (Intersection gl gr) = do
                             l <- reifyGroup gl
                             r <- reifyGroup gr
                             printStr $ "taking intersection "++(show l)++", "++(show r)
                             return (l+r)  

type Comm = Int

data GetGroup  t = GetGroup Comm (Group -> t)
data CComm t = CComm Group (Comm -> t) 

instance Functor GetGroup where
    fmap f (GetGroup i c) = GetGroup i (f . c)

instance Functor CComm where
    fmap f (CComm g c) = CComm g (f . c)

getGroup :: (GetGroup :<: f) => Comm -> Free f Group
getGroup comm = inject (GetGroup comm Pure)

createComm :: (CComm :<: f) => Group -> Free f Comm
createComm group = inject (CComm group Pure)

class EDSLAlgebra f where
    runEDSL :: f (Prog HighExp a) -> Prog HighExp a

instance EDSLAlgebra GetGroup where
    runEDSL (GetGroup i cont) = do
                                    printStr $ "getting group "++(show i)
                                    cont (Get i)

instance EDSLAlgebra CComm where
    runEDSL (CComm group cont) = do
                                    c <- reifyGroup group
                                    printStr $ "creating communicator "++(show c)
                                    cont c

instance (EDSLAlgebra f, EDSLAlgebra g) => EDSLAlgebra (f :+: g) where
    runEDSL (Inl f) = runEDSL f
    runEDSL (Inr g) = runEDSL g

example :: Free (GetGroup :+: CComm) Comm
example = do
            g <- getGroup 1
            g1 <- getGroup 10
            g2 <- getGroup 100
            createComm (Intersection g2 (Union g g1))
