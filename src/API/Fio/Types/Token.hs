{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module API.Fio.Types.Token
  ( Token
  , mkToken
  ) where

import Data.Text (Text)
import Servant.API (ToHttpApiData)
import qualified Data.Text

newtype Token = Token { _unToken :: Text }
  deriving (ToHttpApiData)

-- | Make a token
mkToken :: Text -> Token
mkToken = Token . Data.Text.strip
