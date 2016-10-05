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
import Language.Embedded.Expression
import Language.Embedded.Imperative
import Language.Embedded.Imperative.CMD (RefCMD (GetRef))
import Language.Embedded.Backend.C
import Language.Embedded.CExp

data HighExp a where
  -- Simple constructs
  HVar :: CType a => VarId -> HighExp a
  HLit :: CType a => a -> HighExp a
  HAdd :: (Ord a, Num a, CType a) => HighExp a -> HighExp a -> HighExp a
  HMul :: (Ord a, Num a, CType a) => HighExp a -> HighExp a -> HighExp a
  HNot :: HighExp Bool -> HighExp Bool
  HEq  :: CType a => HighExp a -> HighExp a -> HighExp Bool

  -- Let binding:
  Let  :: CType a
       => HighExp a                 -- value to share
       -> (HighExp a -> HighExp b)  -- body
       -> HighExp b

  -- Pure iteration:
  Iter :: CType s
       => HighExp Int32            -- number of iterations
       -> HighExp s                -- initial state
       -> (HighExp s -> HighExp s) -- step function
       -> HighExp s                -- final state

instance (Ord a, Num a, CType a) => Num (HighExp a) where
  fromInteger = HLit . fromInteger
  (+) = HAdd
  (*) = HMul
