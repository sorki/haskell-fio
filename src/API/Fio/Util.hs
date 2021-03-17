
module API.Fio.Util where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Money (SomeDense)

import qualified Data.Text
import qualified Data.Time.Format
import qualified Money

mkSomeDenseOrError :: Text -> Rational -> SomeDense
mkSomeDenseOrError currency val =
  maybe (error $ "Can't make dense value out of" ++ show val) id
    $ Money.mkSomeDense currency val

parseFioDateOrError :: Text -> Day
parseFioDateOrError x =
  maybe
    (error $ "Unable to parseFioDate: " ++ Data.Text.unpack x)
    id
  $ parseFioDate x

parseFioDate :: Text -> Maybe Day
parseFioDate =
  Data.Time.Format.parseTimeM
    True
    Data.Time.Format.defaultTimeLocale
    "%Y-%-m-%-d"
  . Data.Text.unpack
  . Data.Text.takeWhile (/='+')

printFioDate :: Day -> Text
printFioDate =
  Data.Text.pack
  . Data.Time.Format.formatTime
      Data.Time.Format.defaultTimeLocale
      "%Y-%-m-%-d"
