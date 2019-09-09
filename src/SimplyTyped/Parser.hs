{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}

module SimplyTyped.Parser where

import Data.Text hiding (empty, foldr, foldl')
import Text.Megaparsec hiding (sepEndBy1)
import Data.Void
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import Prelude hiding (unlines)
import Control.Monad (void)
import Data.Char
import Control.Applicative (liftA2)
import Data.List (foldl')

import SimplyTyped.Expression


type Parser = Parsec Void Text
type ParseExp = ParsedTerm Text
type ParseType = Type Text
type ParseAnnVar = AnnVar Text


-- |
-- This ignores spaces and tabs.
spaceP :: Parser ()
spaceP = void $ takeWhileP (Just "space chars") (liftA2 (||) (== ' ') (== '\t'))

-- |
-- This ignores all whitespace.
wSpaceP :: Parser ()
wSpaceP = space1

-- |
wSpaceSomeP :: Parser ()
wSpaceSomeP = C.space

lineComment :: Parser ()
lineComment = empty

blockComment :: Parser ()
blockComment = empty

lexemeS :: Parser a -> Parser a
lexemeS = lexeme spaceP

lexemeSomeWS :: Parser a -> Parser a
lexemeSomeWS = lexeme wSpaceSomeP

lexemeWS :: Parser a -> Parser a
lexemeWS = lexeme wSpaceP

ident :: Parser Text
ident = lexemeS (cons <$> lowerChar <*> takeWhileP Nothing (isAlphaNum))

equals :: Parser ()
equals = void $ lexemeS (char ('=')) 

leftCurly :: Parser ()
leftCurly = void $ lexemeSomeWS (char ('{')) 

rightCurly :: Parser ()
rightCurly = void $ lexemeSomeWS (char ('}'))

leftBracket :: Parser ()
leftBracket = void $ lexemeS (char ('('))

rightBracket :: Parser ()
rightBracket = void $ lexemeSomeWS (char (')'))

bracketed :: Parser a -> Parser a
bracketed p =
  do
    _   <- try leftBracket
    res <- p
    _   <- rightBracket
    pure res

keyword :: Text -> Parser ()
keyword t = void $ lexemeS (chunk t)

var :: Parser ParseExp
var = VarP <$> ident

varT :: Parser ParseType
varT = VarT <$> ident

parseAnnVar :: Parser ParseAnnVar
parseAnnVar = do
  v    <- ident
  void $ lexemeS (char ':')
  t    <- parseType
  pure $ AnnVar v t


parseType :: Parser ParseType
parseType = do
  let arr = void $ lexemeS (chunk "->")
  (t:ts) <- sepBy1 varT arr
  pure $ foldr (:->:) t ts

  
lam :: Parser ParseExp
lam =
  do
    void $ lexemeS (char '\\')
    vars <- some parseAnnVar

    void $ lexemeS (char '.')
    body <- parseExpr
    let lamExpr = foldr LamP body vars
    pure lamExpr


parseUnit :: Parser ParseExp
parseUnit = bracketed parseExpr <|> lam <|> var

app :: Parser ParseExp
app =
  do
    left  <- parseUnit
    rest  <- some parseUnit
    let appExpr = foldl' AppP left rest
    pure appExpr


parseExpr :: Parser ParseExp
parseExpr =
  choice
    [ lam
    , (try app)
    , var
    , bracketed parseExpr
    ]

parseAnnot :: Parser ParseExp
parseAnnot =
  do
    p <- parseExpr
    void $ lexemeS (char ':')
    t <- parseType
    pure $ AnnP p t

    
sepEndBy1 :: Parser a -> Parser sep -> Parser [a]
sepEndBy1 p sep = do{ x <- p
                    ; do{ _ <- sep
                        ; xs <- sepEndBy p sep
                        ; return (x:xs)
                        }
                      <|> return [x]
                    }

