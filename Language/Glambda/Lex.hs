{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Lex
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
--
-- Lexes a Glambda program string into a sequence of tokens
--
----------------------------------------------------------------------------

module Language.Glambda.Lex where

import Prelude hiding ( lex )

import Language.Glambda.Token
import Language.Glambda.Util

import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Pos
import Text.Parsec.Prim  ( Parsec, parse, getPosition )
import Text.Parsec.Error

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token as Parser hiding ( symbolic )

import Control.Error

import Data.Functor
import Data.Functor.Identity
import Data.Text

import Control.Applicative
import Control.Arrow as Arrow
import Data.Maybe

type Lexer = Parsec Text ()

---------------------------------------------------
-- Utility
text_ :: Text -> Lexer ()
text_ = void . text

char_ :: Char -> Lexer ()
char_ = void . char

---------------------------------------------------
-- | Lex some program text into a list of 'LToken's
lex :: Monad m => Text -> EitherT String m [LToken]
lex = hoistEither . Arrow.left show . parse lexer ""

-- | Overall lexer
lexer :: Lexer [LToken]
lexer = (catMaybes <$> many lexer1_ws) <* eof

-- | Lex either one token or some whitespace
lexer1_ws :: Lexer (Maybe LToken)
lexer1_ws
  = (Nothing <$ whitespace)
    <|>
    (Just <$> lexer1)

-- | Lex some whitespace
whitespace :: Lexer ()
whitespace
  = choice [ void $ some space
           , block_comment
           , line_comment ]

-- | Lex a @{- ... -}@ comment (perhaps nested); consumes no input
-- if the target doesn't start with @{-@.
block_comment :: Lexer ()
block_comment = do
  try $ text_ "{-"
  comment_body

-- | Lex a block comment, without the opening "{-"
comment_body :: Lexer ()
comment_body
  = choice [ block_comment *> comment_body
           , try $ text_ "-}"
           , anyChar *> comment_body ]

-- | Lex a line comment
line_comment :: Lexer ()
line_comment = do
  try $ text_ "--"
  void $ manyTill anyChar (eof <|> void newline)

-- | Lex one token
lexer1 :: Lexer LToken
lexer1 = do
  pos <- getPosition
  L pos <$> choice [ symbolic
                   , word_token
                   , Integer <$> Parser.natural ]

-- | Lex one non-alphanumeric token
symbolic :: Lexer Token
symbolic = choice [ LParen  <$  char '('
                  , RParen  <$  char ')'
                  , Lambda  <$  char '\\'
                  , Dot     <$  char '.'
                  , Arrow   <$  try (text "->")
                  , Colon   <$  char ':'
                  , ArithOp <$> arith_op
                  , Assign  <$  char '=' ]

-- | Lex one arithmetic operator
arith_op :: Lexer UArithOp
arith_op = choice [ UArithOp Plus     <$ char '+'
                  , UArithOp Minus    <$ char '-'
                  , UArithOp Times    <$ char '*'
                  , UArithOp Divide   <$ char '/'
                  , UArithOp Mod      <$ char '%'
                  , UArithOp LessE    <$ try (text "<=")
                  , UArithOp Less     <$ char '<'
                  , UArithOp GreaterE <$ try (text ">=")
                  , UArithOp Greater  <$ char '>'
                  , UArithOp Equals   <$ try (text "==")]

-- | Lex one alphanumeric token
word_token :: Lexer Token
word_token = to_token <$> word
  where
    to_token "true"  = Bool True
    to_token "false" = Bool False
    to_token "if"    = If
    to_token "then"  = Then
    to_token "else"  = Else
    to_token other   = Name other

-- | Lex one word
word :: Lexer Text
word = pack <$> ((:) <$> (letter <|> char '_') <*>
                         (many (alphaNum <|> char '_')))
