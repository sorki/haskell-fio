{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Fio.Client (
    tokenFromFile
  , getLast
  , getPeriod
  , getById
  , getLastStatement
  , setLastId
  , setLastDate
  , flushLast
  , getAllTransactions
  ) where

import Data.ByteString (ByteString)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day)
import Money (SomeDense)
import Servant.API
import Servant.Client (BaseUrl(..),ClientM, Scheme(..))

import Control.Monad (void)
import qualified Control.Concurrent (threadDelay)
import qualified Data.Time.Clock
import qualified Data.Text.IO
import qualified Data.Time.Calendar
import qualified Network.HTTP.Client
import qualified Network.HTTP.Client.TLS
import qualified Servant.Client

import API.Fio.Types
import API.Fio.Types.Token

-- type HeaderName = "x-IntervalUseToken"
-- API returns this interval but it doesn't seem
-- to change (always at 20s) so we don't care.

type FioApi =
       "last"
    :> Capture "token" Token
    :> "transactions.json"
    :> Get '[JSON] (AccountStatement SomeDense)
  :<|>
       "periods"
    :> Capture "token" Token
    :> Capture "date-from" Day
    :> Capture "date-to" Day
    :> "transactions.json"
    :> Get '[JSON] (AccountStatement SomeDense)
  :<|>
       "by-id"
    :> Capture "token" Token
    :> Capture "year" Integer
    :> Capture "id" Integer
    :> "transaction.json"
    :> Get '[JSON] (AccountStatement SomeDense)
  :<|>
       "lastStatement"
    :> Capture "token" Token
    :> "statement"
    :> Get '[PlainText] Text
  :<|>
       "set-last-id"
    :> Capture "token" Token
    :> Capture "id" Integer
    :> ""
    :> Get '[OctetStream] ByteString
  :<|>
       "set-last-date"
    :> Capture "token" Token
    :> Capture "date" Day
    :> ""
    :> Get '[OctetStream] ByteString

-- | Get account statement with last transactions
getLast
  :: Token
  -> IO (AccountStatement SomeDense)

-- | Get account statement for @from@ - @to@ period
getPeriod
  :: Token
  -> Day
  -> Day
  -> IO (AccountStatement SomeDense)

-- | Get account statement by year and ID
getById
  :: Token
  -> Integer
  -> Integer
  -> IO (AccountStatement SomeDense)

-- | Get last produced statemnt year and month.
-- Returns `Text` with "yyyy,mm"
getLastStatement
  :: Token
  -> IO Text

-- | Move carret for `last` to some transaction ID
setLastId
  :: Token
  -> Integer
  -> IO ByteString

-- | Move carret for `last` to some Day
setLastDate
  :: Token
  -> Day
  -> IO ByteString

fioApi :: Proxy FioApi
fioApi = Proxy

(getLast
  :<|> getPeriod
  :<|> getById
  :<|> getLastStatement
  :<|> setLastId
  :<|> setLastDate
  ) = Servant.Client.hoistClient
        fioApi
        (runFio 3)
        (Servant.Client.client fioApi)

-- | Read token from file
tokenFromFile :: FilePath -> IO Token
tokenFromFile = fmap mkToken . Data.Text.IO.readFile

-- | Get all transactions.
--
-- Queries `AccountStatement` from 1900-01-01 to tomorrows date.
getAllTransactions :: Token -> IO (AccountStatement SomeDense)
getAllTransactions tok = do
  today <- utctDay <$> Data.Time.Clock.getCurrentTime
  let (thisYear, thisMonth, thisDay) = Data.Time.Calendar.toGregorian today
      tomorrow = Data.Time.Calendar.fromGregorian thisYear thisMonth (thisDay + 1)
      since = Data.Time.Calendar.fromGregorian 1900 1 1

  getPeriod tok since tomorrow

-- | Call `getLast` discarding result, effectively moving
-- carret to the end. Useful after initial sync with `getAllTransactions`.
flushLast :: Token -> IO ()
flushLast = void . getLast

-- | Run FIO Client operation
runFio
  :: Int
  -> ClientM a
  -> IO a
runFio retries f = do
  manager <- Network.HTTP.Client.newManager
    Network.HTTP.Client.TLS.tlsManagerSettings

  r <- Servant.Client.runClientM f
    (Servant.Client.mkClientEnv manager
      (BaseUrl Https host 443 "/ib_api/rest"))

  Control.Concurrent.threadDelay 21666000

  case r of
    Left e -> case retries of
      0 -> error $ show e
      _ -> do
        putStrLn $ "Retrying, attempts remaining" ++ show (retries - 1)
        runFio (retries - 1) f
    Right x -> pure x
  where
    host :: String
    host = "www.fio.cz"
