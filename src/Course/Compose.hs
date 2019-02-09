{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a)) deriving (Show, Eq)

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  f <$> (Compose ds) = Compose  ((f <$>)<$> ds)

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure = Compose . (pure . pure)
-- Implement the (<*>) function for an Applicative instance for Compose
  (Compose f) <*> (Compose ds) =
    Compose (lift2 (<*>) f ds)

instance (Monad m, Monad n) =>
  Monad (Compose m n) where
-- Implement the (=<<) function for a Monad instance for Compose
  f =<< ma = undefined
