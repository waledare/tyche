
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Utils.Types  where 
import Database.Persist.TH
import GHC.Generics
import Data.Aeson hiding (Series, Result)
import Prelude
import Data.Time.Clock (UTCTime)
import Data.Time.Calendar (Day)
import Data.Text (Text)
import Control.Monad
import qualified Data.HashMap.Strict as HM

{-
newtype TimeSeries a = TS {
    observations :: [(Day, Maybe a)]
} deriving (Show, Eq, Read)
-}

data ROut = ROut {
    betas :: [Double],
    sigmas :: [Double],
    capmRsquare :: Double,
    model :: [String]
} deriving (Show, Generic)

instance FromJSON ROut 
instance ToJSON ROut 

data DataStatus = Unavailable | OutDated | UpToDate deriving (Eq, Show)

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
    close :: [Maybe Double]
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



data Chart = Chart {
    symbolName :: Text,
    chart :: Result 
}deriving (Show, Generic)

instance FromJSON Chart

newtype ResponseMsg = ResponseMsp {
    quoteSummary :: Result 
}deriving (Show, Generic)

instance FromJSON ResponseMsg

derivePersistField "MarketData"

data Health = Robust | Average | Fragile deriving (Generic, Show)
instance ToJSON Health 
