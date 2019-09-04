module Untyped.Expression where

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
