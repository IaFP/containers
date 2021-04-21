{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,14,0)
{-# LANGUAGE MagicHash, UnboxedTuples, RankNTypes #-}
{-# LANGUAGE PartialTypeConstructors #-}
#endif

#include "containers.h"
{-# OPTIONS_HADDOCK hide #-}

-- | A clone of Control.Monad.State.Strict.
module Utils.Containers.Internal.State where

import Prelude hiding (
#if MIN_VERSION_base(4,8,0)
    Applicative
#endif
    )

import Control.Monad (ap)
import Control.Applicative (Applicative(..)
#if MIN_VERSION_base(4,14,0)
#else
                           , liftA
#endif
                           )

#if MIN_VERSION_base(4,14,0)
import GHC.Exts (runRW#, noDuplicate#)
import GHC.Types (Total)
#endif

newtype State s a = State {runState :: s -> (s, a)}
#if MIN_VERSION_base(4,14,0)
instance Total (State s)
#else
#endif

#if MIN_VERSION_base(4,14,0)
-- | This is a terrible hack to prevent a thunk from being entered twice.
-- Simon Peyton Jones would very much like to be rid of it.
noDup :: a -> a
noDup a = runRW# (\s ->
  case noDuplicate# s of
    _ -> a)

instance Functor (State s) where
    fmap f m = State $ \ s ->
      let
        -- See Note [Lazy State and multithreading]
        {-# NOINLINE res #-}
        res = noDup (runState m s)
        (new_s, r) = res
      in
        (new_s, f r)

#else
instance Functor (State s) where
    fmap = liftA
#endif

instance Monad (State s) where
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    return = pure
    m >>= k = State $ \ s -> case runState m s of
        (s', x) -> runState (k x) s'

instance Applicative (State s) where
    {-# INLINE pure #-}
    pure x = State $ \ s -> (s, x)
    (<*>) = ap

execState :: State s a -> s -> a
execState m x = snd (runState m x)
