{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module EDSL_Compilation where

import Language.Embedded.Backend.C.Expression (CType)
import Control.Monad.State
import Control.Monad.Writer
import Data.Int
import Data.IORef

data Prog exp a where
  Return :: a -> Prog exp a
  (:>>=) :: Prog exp a -> (a -> Prog exp b) -> Prog exp b
  CMD    :: CMD exp a -> Prog exp a

instance Functor (Prog exp) where
  fmap f m = m >>= return . f

instance Applicative (Prog exp) where
  pure  = return
  (<*>) = ap

instance Monad (Prog exp) where
  return = Return
  (>>=)  = (:>>=)

data CMD exp a where
  -- References:
  InitRef :: Type a => exp a -> CMD exp (Ref a)
  GetRef  :: Type a => Ref a -> CMD exp (Val a)
  SetRef  :: Type a => Ref a -> exp a -> CMD exp ()

  -- Input/output:
  Read     :: CMD exp (Val Int32)
  Write    :: exp Int32 -> CMD exp ()
  PrintStr :: String -> CMD exp ()

  -- Loops:
  For :: exp Int32 -> (Val Int32 -> Prog exp ()) -> CMD exp ()

class    (Eq a, Ord a, Show a, CType a) => Type a
instance (Eq a, Ord a, Show a, CType a) => Type a

data Val a
  = ValRun a       -- Concrete value
  | ValComp VarId  -- Symbolic value

-- Variable identifier
type VarId = String

data Ref a
  = RefRun (IORef a)  -- Concrete reference
  | RefComp VarId     -- Symbolic reference

interpret :: Monad m
          => (forall a . CMD exp a -> m a)
          -> Prog exp a -> m a
interpret int (Return a) = return a
interpret int (p :>>= k) = interpret int p >>= interpret int . k
interpret int (CMD cmd)  = int cmd

class EvalExp exp where
  -- Evaluate a closed expression
  evalExp :: exp a -> a

initRef :: Type a => exp a -> Prog exp (Ref a)
initRef = CMD . InitRef

setRef :: Type a => Ref a -> exp a -> Prog exp ()
setRef r a = CMD (SetRef r a)

class FreeExp exp where
  -- Inject a constant value
  constExp :: Type a => a -> exp a

  -- Inject a variable
  varExp   :: Type a => VarId -> exp a

valToExp :: (Type a, FreeExp exp) => Val a -> exp a
valToExp (ValRun a)  = constExp a
valToExp (ValComp v) = varExp v

getRef :: (Type a, FreeExp exp) => Ref a -> Prog exp (exp a)
getRef = fmap valToExp . CMD . GetRef

readInput :: FreeExp exp => Prog exp (exp Int32)
readInput = valToExp <$> CMD Read

writeOutput :: exp Int32 -> Prog exp ()
writeOutput = CMD . Write

printStr :: String -> Prog exp ()
printStr = CMD . PrintStr

for :: FreeExp exp
    => exp Int32 -> (exp Int32 -> Prog exp ()) -> Prog exp ()
for n body = CMD $ For n (body . valToExp)

modifyRef :: (Type a, FreeExp exp)
          => Ref a -> (exp a -> exp a) -> Prog exp ()
modifyRef r f = setRef r . f =<< getRef r

data LowExp a where
  LVar :: Type a => VarId -> LowExp a
  LLit :: Type a => a -> LowExp a
  LAdd :: (Num a, Type a) => LowExp a -> LowExp a -> LowExp a
  LMul :: (Num a, Type a) => LowExp a -> LowExp a -> LowExp a
  LNot :: LowExp Bool -> LowExp Bool
  LEq  :: Type a => LowExp a -> LowExp a -> LowExp Bool

instance (Num a, Type a) => Num (LowExp a) where
  fromInteger = LLit . fromInteger
  (+) = LAdd
  (*) = LMul

instance EvalExp LowExp where
  evalExp (LLit a)   = a
  evalExp (LAdd a b) = evalExp a + evalExp b
  evalExp (LMul a b) = evalExp a * evalExp b
  evalExp (LNot a)   = not $ evalExp a
  evalExp (LEq a b)  = evalExp a == evalExp b

instance FreeExp LowExp where
  constExp = LLit
  varExp   = LVar

sumInput :: Prog LowExp ()
sumInput = do
  r <- initRef 0
  printStr "Please enter 4 numbers\n"
  for 4 $ \i -> do
    printStr " > "
    n <- readInput
    modifyRef r (+n)
  printStr "The sum of your numbers is "
  s <- getRef r
  writeOutput s
  printStr ".\n"

code :: ShowExp exp => Prog exp a -> Code a
code = interpret codeCMD

lowToCode :: Prog LowExp a -> String
lowToCode = runCode . code

data HighExp a where
  -- Simple constructs (same as in LowExp):
  HVar :: Type a => VarId -> HighExp a
  HLit :: Type a => a -> HighExp a
  HAdd :: (Num a, Type a) => HighExp a -> HighExp a -> HighExp a
  HMul :: (Num a, Type a) => HighExp a -> HighExp a -> HighExp a
  HNot :: HighExp Bool -> HighExp Bool
  HEq  :: Type a => HighExp a -> HighExp a -> HighExp Bool

  -- Let binding:
  Let  :: Type a
       => HighExp a                 -- value to share
       -> (HighExp a -> HighExp b)  -- body
       -> HighExp b

  -- Pure iteration:
  Iter :: Type s
       => HighExp Int32            -- number of iterations
       -> HighExp s                -- initial state
       -> (HighExp s -> HighExp s) -- step function
       -> HighExp s                -- final state

instance (Num a, Type a) => Num (HighExp a) where
  fromInteger = HLit . fromInteger
  (+) = HAdd
  (*) = HMul

instance FreeExp HighExp where
  constExp = HLit
  varExp   = HVar

powerInput :: Prog HighExp ()
powerInput = do
  printStr "Please enter two numbers\n"
  printStr " > "; m <- readInput
  printStr " > "; n <- readInput
  printStr "Here's a fact: "
  writeOutput m; printStr "^"; writeOutput n; printStr " = "
  writeOutput $ Iter n 1 (*m)
  printStr ".\n"

reexpress :: (forall a . exp1 a -> Prog exp2 (exp2 a))
          -> Prog exp1 b
          -> Prog exp2 b
reexpress reexp = interpret (reexpressCMD reexp)

reexpressCMD :: (forall a . exp1 a -> Prog exp2 (exp2 a))
             -> CMD exp1 b
             -> Prog exp2 b
reexpressCMD reexp (InitRef a)  = CMD . InitRef =<< reexp a
reexpressCMD reexp (GetRef r)   = CMD (GetRef r)
reexpressCMD reexp (SetRef r a) = CMD . SetRef r =<< reexp a
reexpressCMD reexp Read         = CMD Read
reexpressCMD reexp (Write a)    = CMD . Write =<< reexp a
reexpressCMD reexp (PrintStr s) = CMD (PrintStr s)
reexpressCMD reexp (For n body) = do
    n' <- reexp n
    CMD $ For n' (reexpress reexp . body)

transHighExp :: HighExp a -> Prog LowExp (LowExp a)
transHighExp (HVar v)   = return (LVar v)
transHighExp (HLit a)   = return (LLit a)
transHighExp (HAdd a b) = LAdd <$> transHighExp a <*> transHighExp b
transHighExp (HMul a b) = LMul <$> transHighExp a <*> transHighExp b
transHighExp (HNot a)   = LNot <$> transHighExp a
transHighExp (HEq a b)  = LEq  <$> transHighExp a <*> transHighExp b

transHighExp (Let a body) = do
  r  <- initRef =<< transHighExp a
  a' <- CMD $ GetRef r
  transHighExp $ body $ valToExp a'

transHighExp (Iter n s body) = do
  n' <- transHighExp n
  r  <- initRef =<< transHighExp s
  for n' $ \_ -> do
    sPrev <- CMD $ GetRef r
    sNext <- transHighExp $ body $ valToExp sPrev
    setRef r sNext
  getRef r

transHighExp' :: HighExp a -> Prog LowExp (LowExp a)

-- Call-by-name:
transHighExp' (Let a body) = transHighExp $ body a

-- Unrolling Iter:
transHighExp' (Iter (HMul n (HLit 2)) s body) = do
  n' <- transHighExp n
  r  <- initRef =<< transHighExp s
  for n' $ \_ -> do
    sPrev <- CMD $ GetRef r
    s1 <- transHighExp $ body $ valToExp sPrev
    setRef r s1
    s1' <- CMD $ GetRef r
    s2 <- transHighExp $ body $ valToExp s1'
    setRef r s2
  getRef r

translateHigh :: Prog HighExp a -> Prog LowExp a
translateHigh = reexpress transHighExp

compile :: Prog HighExp a -> String
compile = lowToCode . translateHigh

-- Code generation monad
type Code = WriterT [Stmt] (State Unique)

type Stmt   = String
type Unique = Integer

runCode :: Code a -> String
runCode = unlines . flip evalState 0 . execWriterT . indent

-- Emit a statement in the generated code
stmt :: Stmt -> Code ()
stmt s = tell [s]

-- Generate a unique identifier
unique :: String -> Code VarId
unique base = do
  u <- get; put (u+1)
  return (base ++ show u)

-- Generate a unique symbolic value
freshVar :: Type a => Code (Val a)
freshVar = ValComp <$> unique "v"

-- Generate a unique reference
freshRef :: Type a => Code (Ref a)
freshRef = RefComp <$> unique "r"

-- Modify a code generator by indenting the generated code
indent :: Code a -> Code a
indent = censor $ map ("    " ++)

-- Code generation of instructions
codeCMD :: ShowExp exp => CMD exp a -> Code a
codeCMD (InitRef a) = do
  r <- freshRef
  stmt $ unwords [show r, "<- initRef", showExp a]
  return r
codeCMD (GetRef r) = do
  v <- freshVar
  stmt $ unwords [show v, "<- getRef", show r]
  return v
codeCMD (SetRef r a) = stmt $ unwords ["setRef", show r, showExp a]
codeCMD Read = do
  v <- freshVar
  stmt $ unwords [show v, "<- readInput"]
  return v
codeCMD (Write a)    = stmt $ unwords ["writeOutput", showExp a]
codeCMD (PrintStr s) = stmt $ unwords ["printStr", show s]
codeCMD (For n body) = do
  i <- freshVar
  stmt $ unwords ["for", show i, "<", showExp n]
  indent $ code (body i)
  stmt "end for"

instance Show (Val a) where show (ValComp a) = a
instance Show (Ref a) where show (RefComp r) = r

class ShowExp exp where
  showExp :: exp a -> String

bracket :: String -> String
bracket s = "(" ++ s ++ ")"

instance ShowExp LowExp where
  showExp (LVar v)   = v
  showExp (LLit a)   = show a
  showExp (LAdd a b) = bracket (showExp a ++ " + " ++ showExp b)
  showExp (LMul a b) = bracket (showExp a ++ " * " ++ showExp b)
  showExp (LNot a)   = bracket ("not " ++ showExp a)
  showExp (LEq a b)  = bracket (showExp a ++ " == " ++ showExp b)
