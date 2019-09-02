module Untyped.Expression


data Expression =
    Let [Block] Expression
  | Var Name
  | Lam Name Expression
  | App Expression Expression
  deriving Show
