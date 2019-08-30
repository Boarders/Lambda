{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser.Lambda.Untyped
import Text.Megaparsec
import Data.Text (Text, unlines)
import Prelude hiding (unlines, putStrLn)
import Data.Foldable
import Data.Text.IO (putStrLn)

main :: IO ()
main = do
  
  parseTestRun lamEx1 lam
  parseTestRun lamEx2 lam
  parseTestRun lamEx3 lam
  parseTestRun lamEx4 lam
  parseTestRun lamEx5 lam
  parseTestRun blockExample parseBlock
  parseTestRun programExample parseProgram



programExample :: Text
programExample = fold ["f = a\n", "g = let {x = b;\n y = c} d\n"]

blockExample :: Text
blockExample = "g = let {x = a; y = b} w"

lamEx1 :: Text
lamEx1 = "\\x . x"

lamEx2 :: Text
lamEx2 = "\\x y . y"

lamEx3 :: Text
lamEx3 = "\\x y . x y"

lamEx4 :: Text
lamEx4 = "\\x y . x y y"

lamEx5 :: Text
lamEx5 = "\\x y . x (y y)"

parseTestRun :: Show a => Text -> Parser a -> IO ()
parseTestRun input parse = do
  putStrLn input
  putStrLn "   parses to give: \n"
  parseTest (parse <* eof) input
  putStrLn ""
  
  
