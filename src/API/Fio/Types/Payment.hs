{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module API.Fio.Types.Payment
    ( Payment(..)
    ) where

import Data.Text (Text)
import Data.Time.Calendar (Day)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text

import Money

import API.Fio.Util

data Payment a = Payment
    { remoteId       :: Int            -- 22
    , date           :: Day            -- 0 formatted as yyyy-mm-dd+nnnn
    , amount         :: a              -- 1
    , currency       :: Text           -- 14 ISO4217
    , remoteAcctNum  :: Maybe Text     -- 2
    , remoteAcctName :: Maybe Text     -- 10
    , remoteBankNum  :: Maybe Text     -- 3
    , remoteBankName :: Maybe Text     -- 12
    , constantSym    :: Maybe Text     -- 4
    , variableSym    :: Maybe Text     -- 5
    , specificSym    :: Maybe Text     -- 6
    , userIdent      :: Maybe Text     -- 7
    , message        :: Maybe Text     -- 16
    , typ            :: Maybe Text     -- 8
    , authorizedBy   :: Maybe Text     -- 9
    , specification  :: Maybe Text     -- 18
    , comment        :: Maybe Text     -- 25
    , bIC            :: Maybe Text     -- 26 ISO9362
    , commandID      :: Maybe Int      -- 17
    } deriving (Eq, Show, Ord, Functor)


instance FromJSON (Payment SomeDense) where
    parseJSON = withObject "Payment" $ \v -> do
        -- let x = epicfail parseColumn
        currency       <- parseColumnRequire 14 v

        remoteId       <- parseColumnRequire 22 v
        amount'        <- parseColumnRequire 1 v :: Parser Rational
        let amount = mkSomeDenseOrError currency amount'
        date'          <- parseColumnRequire 0 v :: Parser Text
        let date = parseFioDateOrError date'
        remoteAcctNum  <- parseColumn 2  v
        remoteAcctName <- parseColumn 10 v
        remoteBankNum  <- parseColumn 3  v
        remoteBankName <- parseColumn 12 v
        constantSym    <- parseColumn 4  v
        variableSym    <- parseColumn 5  v
        specificSym    <- parseColumn 6  v
        userIdent      <- parseColumn 7  v
        message        <- parseColumn 16 v
        typ            <- parseColumn 8  v
        authorizedBy   <- parseColumn 9  v
        specification  <- parseColumn 18 v
        comment        <- parseColumn 25 v
        bIC            <- parseColumn 26 v
        commandID      <- parseColumn 17 v
        return Payment{..}
      where
        parseColumn :: FromJSON a => Int -> Object -> Parser (Maybe a)
        parseColumn num v = do
            mOuter <- v .:? ("column" <> (Data.Text.pack . show $ num))
            case mOuter of
                Nothing -> return Nothing
                Just o  -> do
                    mValue <- o .:? "value"
                    return mValue

        parseColumnRequire :: FromJSON a => Int -> Object -> Parser a
        parseColumnRequire num v = do
            col <- v .: ("column" <> (Data.Text.pack . show $ num))
            col .: "value"
