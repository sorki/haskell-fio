{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API.Fio.Types.AccountStatement
    ( AccountStatement(..)
    , eitherDecodeStatement
    , eitherDecodeStatementFile
    , transactionsBalance
    , validate
    , validateSome
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

import Data.Aeson
import Data.Maybe (isJust)
import qualified Data.Vector

import GHC.TypeLits (KnownSymbol)
import Money

import API.Fio.Types.Payment
import API.Fio.Util

-- | Decode `AccountStatement SomeDense` from `ByteString`
eitherDecodeStatement
  :: ByteString
  -> Either String (AccountStatement SomeDense)
eitherDecodeStatement = eitherDecode

-- | Decode `AccountStatement SomeDense` from file
eitherDecodeStatementFile
  :: FilePath
  -> IO (Either String (AccountStatement SomeDense))
eitherDecodeStatementFile = eitherDecodeFileStrict

data AccountStatement a = AccountStatement
  { accountId      :: Int  -- ^ Account number
  , bankId         :: Int  -- ^ Bank number
  , iban           :: Text -- ^ IBAN code
  , bic            :: Text -- ^ BIC code
  , openingBalance :: a    -- ^ Opening balance
  , closingBalance :: a    -- ^ Closing balance
  , dateStart      :: Day  -- ^ Starting day of this statement
  , dateEnd        :: Day  -- ^ Closing day of this statement
  , yearList       :: Maybe Integer
  , idList         :: Maybe Integer
  , idFrom         :: Maybe Integer -- ^ Contains transaction IDs starting with this ID
  , idTo           :: Maybe Integer -- ^ Contains transaction IDs ending with this ID
  , idLastDownload :: Maybe Integer -- ^ ID of the last downloaded transaction
  , transactions   :: [Payment a]   -- ^ List of transactions
  } deriving (Eq, Show, Ord, Functor, Generic)

instance FromJSON (AccountStatement SomeDense) where
  parseJSON =  withObject "top" $ \v -> do

    as <- v .: "accountStatement"
    i <- as .: "info"

    accountId       <- read <$> i .: "accountId"
    bankId          <- read <$> i .: "bankId"
    currency        <- i .: "currency"
    iban            <- i .: "iban"
    bic             <- i .: "bic"
    openingBalance  <- mkSomeDenseOrError currency <$> i .: "openingBalance"
    closingBalance  <- mkSomeDenseOrError currency <$> i .: "closingBalance"
    dateStart       <- parseFioDateOrError <$> i .: "dateStart"
    dateEnd         <- parseFioDateOrError <$> i .: "dateEnd"
    yearList        <- i .:? "yearList"
    idList          <- i .:? "idList"
    idFrom          <- i .:? "idFrom"
    idTo            <- i .:? "idTo"
    idLastDownload  <- i .:? "idLastDownload"

    transactions <-
          as .: "transactionList"
      >>= (.: "transaction")
      >>= (withArray "arr" $ \a -> mapM parseJSON (Data.Vector.toList a))

    return AccountStatement{..}

-- | Compute sum of all transactions, provided
-- you know the currency.
transactionsBalance
  :: forall c . KnownSymbol c
  => AccountStatement (Maybe (Dense c))
  -> Maybe (Dense c)
transactionsBalance =
  foldl
    (\acc t -> (+) <$> acc <*> amount t)
    (dense @c 0)
  . transactions

-- | Validate that @@opening + transactions = closing@@ balance.
--
-- If you know the currency, you can convert
-- @AccountStatement SomeDense@ to
-- @AccountStatement (Maybe (Dense currency))
-- using @fromDense @currency <@> as@.
validate
  :: forall c . KnownSymbol c
  => AccountStatement (Maybe (Dense c))
  -> Bool
validate ac =
  ((+) <$> openingBalance ac <*> transactionsBalance ac)
  == closingBalance ac && isJust (closingBalance ac)

-- | Validate that @@opening + transactions = closing@@ balance.
--
-- Variant of `validate` that works with @AccoutStatement SomeDense@.
validateSome
  :: AccountStatement SomeDense
  -> Bool
validateSome ac' =
  withSomeDense (openingBalance ac') $ \(_ :: Dense currency) ->
    let ac = fromSomeDense @currency <$> ac'
    in validate ac
