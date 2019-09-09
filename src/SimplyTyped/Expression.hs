{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module SimplyTyped.Expression where

data Kind = Star
  deriving stock (Eq)

instance Show Kind where
  show Star = "✱"

data Type a =
    Type a :->: Type a
  | VarT a

instance Show a => Show (Type a) where
  show (VarT a) = show a
  show (v :->: t) = unwords [show v,  "→", show t]

newtype Scope f a = Scope {runScope :: f a}
--  deriving newtype (Functor, Foldable, Traversable)

data AnnVar a = AnnVar a (Type a)
  deriving Show

data ParsedTerm a =
    VarP a
  | AnnP (ParsedTerm a) (Type a)
  | AppP (ParsedTerm a) (ParsedTerm a)
  | LamP (AnnVar a) (ParsedTerm a)
  deriving Show


data TermInf a =
    FVar a
  | BVar Int
  | Ann (TermInf a) (Type a)
  | App (TermInf a) (TermInf a)

data TermCheck a =
    Inf (TermInf a)
  | Lam (TermCheck a)


--type ProgramF a = [BlockF ExpressionF a]
--type BlockF l a  = (a, l a)
