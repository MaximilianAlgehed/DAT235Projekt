{-# LANGUAGE GADTs, TypeOperators, FlexibleContexts, FlexibleInstances #-}
import UsingImperativeEDSL
import DatatypesAlacarte
import EDSL_Compilation
import Control.Monad
import Language.Embedded.Expression
import Language.Embedded.Imperative hiding ((:+:), (:<:))
import Language.Embedded.Imperative.CMD (RefCMD (GetRef))
import Language.Embedded.Backend.C
import Language.Embedded.CExp

-- Groups represented naïvely
--
-- the problem with this is that
-- the expressions are not from the
-- rest of the program.
--
-- You can't even write:
--  g <- do
--          iff condition
--              (get 5)
--              (get 6)
--  becuase it now becomes expressions in the
--  Data syntax ...
data Group where
    Get          :: Int -> Group
    Union        :: Group -> Group -> Group
    Intersection :: Group -> Group -> Group

-- The idea here is that we pass around the
-- expression that frees up interm groups untill
-- the very end
--
-- obviously this is subject to the sharing problem,
-- which needs to be fixed for a solution like this one to be viable.
reifyGroup' :: Group -> Prog HighExp (Prog HighExp Int, Prog HighExp ())
reifyGroup' (Get i) = return (return i, return ())
reifyGroup' (Union gl gr) = do
                                 (ml, cl) <- reifyGroup' gl
                                 (mr, cr) <- reifyGroup' gr
                                 l <- ml
                                 r <- mr
                                 printf $ "creating group "++(show (l+r))
                                 printf $ "taking union "++(show l)++", "++(show r)
                                 cl
                                 cr
                                 return (return (l+r),  printf $ "deleting group "++(show (l+r)))
reifyGroup' (Intersection gl gr) = do
                                 (ml, cl) <- reifyGroup' gl
                                 (mr, cr) <- reifyGroup' gr
                                 l <- ml
                                 r <- mr
                                 printf $ "creating group "++(show (l+r))
                                 printf $ "taking taking intersection "++(show l)++", "++(show r)
                                 cl
                                 cr
                                 return (return (l+r), printf $ "deleting group "++(show (l+r)))

reifyGroup g = join $ fmap fst $ reifyGroup' g
               
type Comm = Int

data GetGroup t = GetGroup Comm (Group -> t)
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
    runEDSL :: f (Program CMD (Param2 HighExp CType) a) -> Program CMD (Param2 HighExp CType) a

instance EDSLAlgebra GetGroup where
    runEDSL (GetGroup i cont) = do
                                    printf $ "getting group "++(show i)
                                    cont (Get i)

instance EDSLAlgebra CComm where
    runEDSL (CComm group cont) = do
                                    c <- reifyGroup group
                                    printf $ "creating communicator "++(show c)
                                    cont c

instance EDSLAlgebra (Program CMD (Param2 HighExp CType)) where
    runEDSL = join 

instance (EDSLAlgebra f, EDSLAlgebra g) => EDSLAlgebra (f :+: g) where
    runEDSL (Inl f) = runEDSL f
    runEDSL (Inr g) = runEDSL g

-- The next goal is obviously to get rid of the need for this ugly thing!
-- which can be done by lifting the whole interface function by function.
-- Such a lifting would not be a problem as it would only need to be done
-- once!
free :: (Program CMD (Param2 HighExp CType) :<: f) => Program CMD (Param2 HighExp CType) a -> Free f a
free prog = inject $ prog >>= (\x -> return $ Pure x)

-- To note is that this examples mixes the "GetGroup" and "CComm" extensions to the (Program ...) monad
--
-- We would like to have a type that looks something like:
-- (HighExp :<: groupExp, groupExp :~>: Prog HighExp, HighExp :<: ccommExp, ccommExp :~>: Prog HighExp) =>
-- Free (GetGroup groupExp :+: CComm ccommExp :+: Program CMD ...) ()
--
-- or something like that. We would like to capture that HighExp and CType etc. lift to the
-- high level DSLs. One difficulty is how to connect the two extensions, do you require that
-- there is a bijection between groupExp and ccommExp for an instance?
-- Is there a way for the user to choose how much interaction the two DSLs can have etc.
--
-- Thoughts?
example :: Free (GetGroup :+: CComm :+: Program CMD (Param2 HighExp CType)) ()
example = do
            g <- getGroup 1
            g1 <- getGroup 10
            g2 <- getGroup 100
            free $ printf "You can, with some effort, put Prog HighExp statements in here too!"
            createComm (Intersection g2 (Union g g1))
            return ()

compileFree :: (EDSLAlgebra f, Functor f) => Free f a -> Program CMD (Param2 HighExp CType) a
compileFree = foldFree return runEDSL
