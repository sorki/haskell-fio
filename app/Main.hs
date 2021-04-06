module Main where

import Control.Monad
import System.Environment
import API.Fio

import Text.Pretty.Simple

main :: IO ()
main = do
  tokPath <- getEnv "FIO_TOKEN_PATH"
  tok <- tokenFromFile tokPath

  as <- getAllTransactions tok
  pPrint as

  flushLast tok
  forever $ getLast tok >>= pPrint

