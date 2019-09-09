{-# LANGUAGE OverloadedStrings #-}
module Main where

import SimplyTyped.Parser
import Text.Megaparsec
import Data.Text (Text, unlines)
import Prelude hiding (unlines, putStrLn)
import Data.Foldable
import Data.Text.IO (putStrLn)


main :: IO ()
main = do

  parseTestRun lamEx1 parseExpr
  parseTestRun lamEx2 parseExpr
  parseTestRun lamEx3 parseExpr
  parseTestRun lamEx4 parseExpr
  parseTestRun lamEx5 parseExpr
  parseTestRun lamEx6 parseExpr
--  parseTestRun blockExample parseBlock
--  parseTestRun programExample parseProgram



lamEx1 :: Text
lamEx1 = "\\x : a . x"

lamEx2 :: Text
lamEx2 = "\\x : a y : b . y"

lamEx3 :: Text
lamEx3 = "\\x : a y : b . x y"

lamEx4 :: Text
lamEx4 = "\\x : a y : b . x y y"

lamEx5 :: Text
lamEx5 = "\\x : a y : b . x (y y)"

lamEx6 :: Text
lamEx6 = "\\x : a y : b . (x (x y))"


parseTestRun :: Show a => Text -> Parser a -> IO ()
parseTestRun input parse = do
  putStrLn input
  putStrLn "   parses to give: \n"
  parseTest (parse <* eof) input
  putStrLn ""
  
  
