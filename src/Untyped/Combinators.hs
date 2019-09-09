{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Untyped.Combinators where

import Untyped.Expression

import Data.Set

data SKI = S | K | I
  deriving Show

data SKIExpression a =
    SKIVar a
  | SKILam a (SKIExpression a)
  | SKIApp   (SKIExpression a) (SKIExpression a)
  | SKI  SKI

freeVars :: (Ord a) => SKIExpression a -> Set a
freeVars (SKIVar a)      = singleton a
freeVars (SKILam b body) = b `delete` (freeVars body)
freeVars (SKIApp l r)  = (freeVars l) `union` (freeVars r)
freeVars (SKI _)     = mempty

toSKIRep :: (Ord a) => ExpressionF a -> SKIExpression a
toSKIRep = eliminateBoundVars . toSKI

-- TODO : let expressions
printSKI :: (Show a) => SKIExpression a -> String
printSKI (SKIVar v)      = show v
printSKI (SKILam b body) = unwords ["λ", show b , "→ ", printSKI body]
printSKI (SKIApp l r)    = unwords ["(" <> printSKI l, printSKI r <> ")"]
printSKI (SKI ski) = show ski


elimBoundVars :: (Ord a) => SKIExpression a -> SKIExpression a
elimBoundVars = \case
  SKILam v (SKIVar v') | v == v' -> SKI I
  SKILam v e | v `notMember` freeVars e -> SKIApp (SKI K) (elimBoundVars e)
  SKILam v (SKIApp l r)
    -> elimBoundVars (SKIApp (SKIApp (SKI S) (SKILam v l)) (SKILam v r))
  SKILam v body -> elimBoundVars (SKILam v (elimBoundVars body))
  (SKIApp l r) -> SKIApp (elimBoundVars l) (elimBoundVars r)
  ski          -> ski


toSKI :: ExpressionF a -> SKIExpression a
toSKI (Var v)   = (SKIVar v)
toSKI (App l r) = SKIApp (toSKI l) (toSKI r)
toSKI (Lam b f) = SKILam b (toSKI f)

  

abstract :: (Eq a) => a -> SKIExpression a -> SKIExpression a
abstract var (SKIVar name)
  | var == name = SKI I
  | otherwise   = SKIApp (SKI K) (SKIVar name)
abstract var (SKIApp l r) = SKIApp (SKIApp (SKI S) (abstract var l)) (abstract var r)
abstract _ ski = SKIApp (SKI K) ski


eliminateBoundVars :: (Eq a) => SKIExpression a -> SKIExpression a
eliminateBoundVars = \case
  SKILam a body -> abstract a (eliminateBoundVars body)
  SKIApp l r    -> SKIApp (eliminateBoundVars l) (eliminateBoundVars r)
  expression    -> expression
  

