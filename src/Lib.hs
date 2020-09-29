{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
  ( emptyD,
    fact,
    pureD,
    calculate,
    apply,
    lift,
    validate,
    given,
    eval,
    Depend (..),
  )
where

import Control.Applicative (Alternative, empty)
import Control.Monad (MonadPlus, guard)
import Data.Kind

{-  A simple expression language contract.
    Later we can add serialisation/notes/help/etc. -}
class Expression (e :: Type -> Type -> Type) where
  exprId :: e a a -- identity expression
  exprCompose :: e b c -> e a b -> e a c -- chain expression
  exprUnlift :: e a b -> (a -> b) -- convert expression in to ordinary function

{-  For now, a basic function can be a stand-in for our expressions.  Obviously we can't
    serialise compiled functions in a general way -}
instance Expression (->) where
  exprId = id
  exprCompose = (.)
  exprUnlift = id

{- TODO: Make an expression language for something like EXCEL -}

{-  type Depend = (Type -> Type) -> Type -> Type -}
{-  m is some monad,
    e is some expression language,
    a is the return value -}
data Depend (m :: Type -> Type) (e :: Type -> Type -> Type) a where
  Empty :: Depend m e a
  Pure :: a -> Depend m e a
  Apply :: Expression e => Depend m e (e a b) -> Depend m e a -> Depend m e b
  Guard :: Expression e => Depend m e (e a Bool) -> Depend m e a -> Depend m e a
  Given :: Expression e => Depend m e (e a Bool) -> Depend m e a -> Depend m e b -> Depend m e b

-- also Empty === mempty, Pure === pure, Apply === Ap

{-  Next follows the functions that wrap our constructors.
    While we export the whole GADT, mostly the functions below are the real entry points.
    It's much easier to add a new function than rename a GADT constructor and all it's pattern matches  -}
emptyD :: Depend m e a
emptyD = Empty

pureD, fact :: a -> Depend m e a
fact = Pure
pureD = fact

apply, calculate :: Expression e => Depend m e (e a b) -> (Depend m e a -> Depend m e b)
calculate = Apply
apply = calculate

{-- ```lift``` is included  here as a crutch to get built-in functions into our algebra.
    Instead, we should build an expression language --}
lift :: (a -> b) -> Depend m (->) a -> Depend m (->) b
lift = liftExpr

liftExpr :: Expression e => e a b -> Depend m e a -> Depend m e b
liftExpr = calculate . fact

validate :: Expression e => Depend m e (e a Bool) -> Depend m e a -> Depend m e a
validate = Guard

given :: Expression e => Depend m e (e a Bool) -> Depend m e a -> Depend m e b -> Depend m e b
given = Given

instance Semigroup a => Semigroup (Depend m (->) a) where
  da <> db = calculate (lift (<>) da) db

instance Semigroup (Depend m e a) => Monoid (Depend m e a) where
  mempty = emptyD

-- then some helpers:

-- continuation style.  We can't extract a, but we can extract the result of applying a function to a
-- MonadPlus allows us to short circuit
eval :: MonadPlus m => (a -> r) -> Depend m (->) a -> m r
eval f Empty = empty
eval f (Pure a) = pure $ f a
eval f (Apply dg da) = do
  fg <- eval (f .) dg
  eval fg da
eval f (Guard dg da) = do
  g <- eval id dg
  a <- eval id da
  guard (g a)
  pure $ f a
eval f (Given dg da db) = do
  g <- eval id dg
  ga <- eval g da
  guard ga
  eval f db
