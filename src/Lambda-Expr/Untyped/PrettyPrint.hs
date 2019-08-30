{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Untyped.PrettyPrint
  ( printExpr
  )
  where

import Parser.Lambda.Untyped
import Data.Foldable
import Data.Text (unwords, Text, replicate)
import Prelude hiding (unwords, replicate)

printIdent :: Identifier -> Text
printIdent = id

printBlock :: Block -> Text
printBlock (ident, expr) =
  fold
    [ (printIdent ident)
    , " = "
    , printExpr 0 expr
    ]

putSpaces :: Int -> Text
putSpaces n = replicate n " "


printExpr :: Int -> Expression -> Text
printExpr indent =
  \case
    Var var -> var
    Let block body -> undefined
    App l r -> unwords [printExpr 0 l, printExpr 0 r]
    Lam name body -> unwords ["λ", name, "→", printExpr 0 body]
    
    

