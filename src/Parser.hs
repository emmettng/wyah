{-# LANGUAGE OverloadedStrings #-}

module Parser
    (
    ) where

import LispVal

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang

import qualified Data.Text as T

import Control.Applicative hiding ((<|>))       -- WHAT IS THIS 4 ?
import Data.Functor.Identity (Identity)         -- WHAT IS THIS 4 ?

lexer ::  Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style  = Lang.emptyDef{
    Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.commentLine = "--"
  , Tok.opStart = Tok.opLetter style
  , Tok.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~"
  , Tok.identStart = letter <|> oneOf "-+/*=|&><"
  , Tok.identLetter = digit <|> letter <|> oneOf "?+=|&-/"
  , Tok.reservedOpNames = ["'", "\""]
  }

Tok.TokenParser { Tok.parens = m_parens
           , Tok.identifier = m_identifier } = Tok.makeTokenParser style
