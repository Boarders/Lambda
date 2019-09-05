{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Untyped.Expression where

import Data.Kind
import Data.Text

data ExpressionF a =
    Let [BlockF ExpressionF a] (ExpressionF a)
  | Var a
  | Lam a (ExpressionF a)
  | App (ExpressionF a) (ExpressionF a)
  deriving Show

class LambdaExpression l where
  variable    :: l a
  application :: l a -> l a -> l a
  lambda      :: a -> l a -> la 

type ProgramF a = [BlockF ExpressionF a]
type BlockF l a  = (a, l a)


type Identifier = Text
type Expression = ExpressionF Identifier
type Program    = ProgramF Identifier
type Block      = BlockF ExpressionF Identifier


data Var b a =
    B b
  | F a
  deriving (Eq, Ord, Show)

newtype Scope
          (b :: Type)
          (f :: k -> Type -> Type)
          (i :: k)
          (a :: Type)
  = Scope {getScope :: f i (Var b (f i a))}

data ExpX i a
  = LitX (XLit i a)
  | BVarX (XBVar i a)
  | FVarX (XFVar i a)
  | LamX (XAbs i a) (ScopeX (XBVar i a) ExpX i a)
  | AppX (XApp i a) (ExpX i a) (ExpX i a)
  | ExpX (XExp i a)


type family XLit i a
type family XBVar i a
type family XFVar i a
type family XAbs i a
type family XApp i a
type family XExp i a
type family ScopeX (b :: Type) (f :: k -> Type -> Type) (i :: k) (a :: Type) :: Type

data Named
data DeBruijn
data XSKI

type instance XLit Named a = ()
type instance XFVar Named a = a
type instance XBVar Named a = a
type instance XAbs Named a = a
type instance XApp Named a = ()
type instance XExp Named a = ()
type instance ScopeX b f Named a = f Named a


type instance XLit  Named a = ()
type instance XFVar Named a = a
type instance XBVar Named a = a
type instance XAbs Named a = a
type instance XApp Named a = ()
type instance XExp Named a = ()
type instance ScopeX b f Named a = f Named a





