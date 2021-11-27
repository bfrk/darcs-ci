{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Darcs.Util.IndexedMonad
  ( Monad(..), LiftIx(..), when, ifThenElse
  , MonadReader(..), ReaderT(..), asks
  ) where

import Darcs.Prelude hiding ( Monad(..) )

-- This is required to implement the "if then else" syntax
-- because we are using RebindableSyntax.
-- It doesn't currently exist anywhere standard: see
-- https://gitlab.haskell.org/ghc/ghc/-/issues/18081
-- It doesn't strictly belong in this module but in practice
-- we only use RebindableSyntax to allow us to use the
-- indexed monad class.
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _ = t
ifThenElse False _ e = e

-- At the moment the code is organised into different modules partially to
-- separate it by which Monad class we want (normal or indexed). Once qualified
-- do-notation is available (i.e. min GHC is 9.0) we can stop doing that.
-- |An alternative monad class, indexed by a "from" and "to" state.
class Monad m where
  return :: a -> m i i a
  (>>=) :: m i j a -> (a -> m j k b) -> m i k b
  (>>) :: m i j a -> m j k b -> m i k b

when :: Monad m => Bool -> m i i () -> m i i ()
when b m = if b then m else return ()

-- |A class for indexed monad transformers, going from normal Haskell monads
-- into our indexed monads.
class LiftIx t where
  liftIx :: m a -> t m i i a

-- |An indexed version of the standard 'MonadReader' class
class Monad m => MonadReader r m | m -> r where
  ask :: m i i r
  local :: (r -> r) -> m i i a -> m i i a

asks :: MonadReader r m => (r -> a) -> m i i a
asks f = ask >>= \r -> return (f r)

-- |An indexed version of the standard 'ReaderT' transformer
newtype ReaderT r m i j a = ReaderT { runReaderT :: r -> m i j a }

instance Monad m => Monad (ReaderT r m) where
  return v = ReaderT (\_ -> return v)
  ReaderT m >>= f = ReaderT (\r -> m r >>= \a -> runReaderT (f a) r)
  ReaderT m >> ReaderT n = ReaderT (\r -> m r >> n r)

instance Monad m => MonadReader r (ReaderT r m) where
  ask = ReaderT return
  local f (ReaderT m) = ReaderT (m . f)
