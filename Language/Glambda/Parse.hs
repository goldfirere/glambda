{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Glambda.Parse
-- Copyright   :  (C) 2015 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (eir@cis.upenn.edu)
-- Stability   :  experimental
--
-- Parses tokens into the un-type-checked AST. "Parsing", in glambda,
-- also includes name resolution. This all might
-- conceivably be done in a later pass, but there doesn't seem to be
-- an incentive to do so.
--
----------------------------------------------------------------------------

module Language.Glambda.Parse ( parseStmtG, parseExpG, parseStmt, parseExp ) where

import Language.Glambda.Unchecked
import Language.Glambda.Statement
import Language.Glambda.Token
import Language.Glambda.Type
import Language.Glambda.Monad

import Text.Parsec.Prim as Parsec   ( runParserT, ParsecT, tokenPrim )
import Text.Parsec.Error            ( ParseError )
import Text.Parsec.Pos

import Text.Parser.Combinators as Parser

import Text.PrettyPrint.HughesPJClass

import Control.Error

import Data.List as List
import Data.Text as Text

import Control.Applicative
import Control.Arrow as Arrow ( left )
import Control.Monad.Reader

-- | Parse a 'Statement', aborting with an error upon failure
parseStmtG :: [LToken] -> GlamE Statement
parseStmtG = eitherToGlamE . parseStmt

-- | Parse a 'Statement'
parseStmt :: [LToken] -> Either String Statement
parseStmt = parse stmt

-- | Parse a 'UExp', aborting with an error upon failure
parseExpG :: [LToken] -> GlamE UExp
parseExpG = eitherToGlamE . parseExp

-- | Parse a 'UExp'
parseExp :: [LToken] -> Either String UExp
parseExp = parse expr

parse :: Parser a -> [LToken] -> Either String a
parse p tokens = Arrow.left show $
                 runReader (runParserT (p <* eof) () "" tokens) []

----------------------
-- Plumbing

-- the "state" is a list of bound names. searching a bound name in the list
-- gives you the correct deBruijn index
type Parser = ParsecT [LToken] () (Reader [Text])

-- | Bind a name over an expression
bind :: Text -> Parser a -> Parser a
bind bound_var thing_inside
  = local (bound_var :) thing_inside

-- | Parse the given nullary token
tok :: Token -> Parser ()
tok t = tokenPrim (render . pPrint) next_pos (guard . (t ==) . unLoc)

-- | Parse the given unary token
tok' :: (Token -> Maybe thing) -> Parser thing
tok' matcher = tokenPrim (render . pPrint) next_pos (matcher . unLoc)

-- | Parse one of a set of 'ArithOp's
arith_op :: [UArithOp] -> Parser UArithOp
arith_op ops = tokenPrim (render . pPrint) next_pos
                         (\case L _ (ArithOp op) | op `elem` ops -> Just op
                                _                                -> Nothing)

next_pos :: SourcePos  -- ^ position of the current token
         -> LToken     -- ^ current token
         -> [LToken]   -- ^ remaining tokens
         -> SourcePos  -- ^ location of the next token
next_pos pos _ []            = pos
next_pos _   _ (L pos _ : _) = pos

--------------
-- Real work

stmt :: Parser Statement
stmt = choice [ try $ NewGlobal <$> tok' unName <* tok Assign <*> expr
              , BareExp <$> expr ]

expr :: Parser UExp
expr = choice [ lam
              , cond
              , int_exp `chainl1` bool_op ]

int_exp :: Parser UExp
int_exp = term `chainl1` add_op

term :: Parser UExp
term = apps `chainl1` mul_op

apps :: Parser UExp
apps = List.foldl1 UApp <$> some factor

factor :: Parser UExp
factor = choice [ between (tok LParen) (tok RParen) expr
                , UIntE <$> tok' unInteger
                , UBoolE <$> tok' unBool
                , var ]

lam :: Parser UExp
lam = do
  tok Lambda
  bound_var <- tok' unName
  tok Colon
  typ <- ty
  tok Dot
  e <- bind bound_var $ expr
  return (ULam typ e)

cond :: Parser UExp
cond = UCond <$ tok If <*> expr <* tok Then <*> expr <* tok Else <*> expr

var :: Parser UExp
var = do
  n <- tok' unName
  m_index <- asks (elemIndex n)
  case m_index of
    Nothing -> return (UGlobal n)
    Just i  -> return (UVar i)

ty :: Parser Ty
ty = chainr1 arg_ty (Arr <$ tok Arrow)

arg_ty :: Parser Ty
arg_ty = choice [ between (tok LParen) (tok RParen) ty
                , TyCon <$> tycon ]

tycon :: Parser TyCon
tycon = do
  n <- tok' unName
  case readTyCon n of
    Nothing -> unexpected $ render $
               text "type" <+> quotes (text (unpack n))
    Just tc -> return tc

add_op, mul_op, bool_op :: Parser (UExp -> UExp -> UExp)
add_op = mk_op <$> arith_op [uPlus, uMinus]
mul_op = mk_op <$> arith_op [uTimes, uDivide, uMod]
bool_op = mk_op <$> arith_op [uLess, uLessE, uGreater, uGreaterE, uEquals]

mk_op :: UArithOp -> UExp -> UExp -> UExp
mk_op op = \e1 e2 -> UArith e1 op e2
