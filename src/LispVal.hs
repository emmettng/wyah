{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module LispVal
    (
    ) where

import Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader

type EnvCtx = Map.Map T.Text LispVal

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool deriving (Typeable)

newtype IFunc  = IFunc { fn :: [LispVal] -> Eval LispVal}

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
  deriving ( Monad
           , Functor
           , Applicative
           , MonadReader EnvCtx
           , MonadIO)

instance Show LispVal where
  show = T.unpack . showEval

showEval :: LispVal -> T.Text
showEval val =
  case val of
    (Atom atom) -> atom
    (Number num) -> T.pack . show $ num
    (String txt) -> T.concat ["\"", txt, "\""]
    (Fun _) -> "(Internal Function.)"
    (Lambda _ _) -> "(Lambda function.)"
    (List contents) -> T.concat ["(",T.unwords $ showEval <$> contents,")"]
    (Bool True) -> "#t"
    (Bool False) -> "#f"
    Nil -> "Nil"
