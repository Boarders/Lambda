module Untyped.Expression where

import Data.Text

data Expression =
    Let [Block] Expression
  | Var Name
  | Lam Name Expression
  | App Expression Expression
  deriving Show

type Identifier = Text
type Name  = Text
type Program = [Block]
type Block = (Identifier, Expression)
