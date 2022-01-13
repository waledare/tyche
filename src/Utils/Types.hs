{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Utils.Types where 
import Database.Persist.TH
import GHC.Generics
import Data.Aeson hiding (Result)
import Prelude
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Control.Monad
import qualified Data.HashMap.Strict as HM



type Exchange = Text

data Listing = Listing {
    nasdaqTraded :: Bool,
    symbol :: Text,
    securityName :: Text,
    exchange :: Text,
    marketCategory :: Text,
    etf :: Bool,
    lotSize :: Text,
    testIssue :: Bool,
    financialStatus :: Text,
    cqsSymbol :: Text,
    nasdaqSymbol :: Text,
    nextShares :: Text
} deriving (Show, Read, Generic)

instance ToJSON Listing
instance FromJSON Listing

data UniverseSelector = ADR | Rest | All deriving (Show)

data Position = Position {
    listing :: Listing,
    quantity :: Integer
} deriving (Show, Read, Generic)

instance ToJSON Position 
instance FromJSON Position

derivePersistField "Position"

data Raw = Raw { fmt :: Maybe Text,
                 longFmt :: Maybe Text,
                 raw :: Maybe Double} 
           deriving (Read, Show, Generic)
instance FromJSON Raw {-where
    parseJSON (Object v) = Raw <$>
                           v .:? "fmt" <*>
                           v .:? "bookValue" <*>
                           v .:? "priceToBook"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero -}

newtype  Result = Result {
    result :: [MarketData] 
}deriving (Show, Generic)

instance FromJSON Result

newtype Ohlc = Ohlc {
    close :: [Double]
}deriving (Show, Read, Generic)

instance FromJSON Ohlc
instance ToJSON Ohlc

newtype Indicator = Indicator {
    quote :: [Ohlc]
}deriving (Read , Show, Generic)

instance FromJSON Indicator
instance ToJSON Indicator

data MarketData = MarketData {
    timestamp :: [Integer],
    indicators :: Indicator
} deriving (Show, Generic, Read)


instance FromJSON MarketData
instance ToJSON MarketData

newtype Chart = Chart {
    chart :: Result 
}deriving (Show, Generic)

instance FromJSON Chart

newtype ResponseMsg = ResponseMsp {
    quoteSummary :: Result 
}deriving (Show, Generic)

instance FromJSON ResponseMsg



derivePersistField "MarketData"
