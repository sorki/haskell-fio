module Main where

import Control.Monad
import System.Environment
import API.Fio

main :: IO ()
main = do
  tokPath <- getEnv "FIO_TOKEN_PATH"
  tok <- tokenFromFile tokPath

  as <- getAllTransactions tok
  print as

  flushLast tok
  forever $ getLast tok >>= print

