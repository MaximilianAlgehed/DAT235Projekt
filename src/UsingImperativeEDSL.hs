{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- This code implements the languages from EDSL_Compilation.lhs using the
-- generic imperative-edsl library.
--
-- Instead of `LowExp` we're using the existing type `CExp` from
-- imperative-edsl.
--
-- Note that this implementation produces actual C code rather than pseudo-code.

module UsingImperativeEDSL where

import Data.Int

import Control.Monad.Operational.Higher (singleInj, reexpress)

import Language.Embedded.Expression
import Language.Embedded.Imperative
import Language.Embedded.Imperative.CMD (RefCMD (GetRef))
import Language.Embedded.Backend.C
import Language.Embedded.CExp

import EDSL_Compilation (HighExp (..))

type CMD
    =   RefCMD      -- Mutable references
    :+: ControlCMD  -- Control structures
    :+: FileCMD     -- Input/output

type Prog exp a = Program CMD (Param2 exp CType) a

instance FreeExp HighExp where
    type FreePred HighExp = CType
    varExp = HVar
    constExp = HLit

transHighExp :: HighExp a -> Prog CExp (CExp a)
transHighExp (HVar v)   = return (varExp v)
transHighExp (HLit a)   = return (constExp a)
transHighExp (HAdd a b) = (+)   <$> transHighExp a <*> transHighExp b
transHighExp (HMul a b) = (*)   <$> transHighExp a <*> transHighExp b
transHighExp (HNot a)   = not_  <$> transHighExp a
transHighExp (HEq a b)  = (#==) <$> transHighExp a <*> transHighExp b
transHighExp (Let a body) = do
  r  <- initRef =<< transHighExp a
  a' <- singleInj $ GetRef r
  transHighExp $ body $ valToExp a'
transHighExp (Iter n s body) = do
  n' <- transHighExp n
  sr <- initRef =<< transHighExp s
  for (0,1,Excl n') $ \_ -> do
    sPrev <- singleInj $ GetRef sr
    sNext <- transHighExp $ body $ valToExp sPrev
    setRef sr sNext
  getRef sr

comp :: Prog HighExp a -> String
comp = compile . reexpress transHighExp

powerInput :: Prog HighExp ()
powerInput = do
  printf "Please enter two numbers\n"
  printf " > "; m :: HighExp Int32 <- fget stdin
  printf " > "; n :: HighExp Int32 <- fget stdin
  printf "Here's a fact: %d^%d = %d\n" m n (Iter n 1 (*m))

--main = putStrLn $ comp powerInput
