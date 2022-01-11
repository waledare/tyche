{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Utils.Types where 
import Database.Persist.TH
import GHC.Generics
import Data.Aeson
import Prelude
import Data.Text (Text)

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
