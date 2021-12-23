{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Utils.Yahoo.Portfolio where

import Import (Handler, liftIO ,  appSettings, getYesod)
import Utils.Yahoo.Crumb
import Settings  
import Data.Text (Text, unpack)
import Network.HTTP.Conduit hiding (Response)
import Data.Aeson hiding (Result)
import GHC.Generics
import Control.Monad
import Control.Exception (try)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.HashMap.Strict as HM

yahooSettings :: Handler YahooConfig
yahooSettings = appYahooConfig . appSettings <$> getYesod

type Module = Text
type Symbol = Text


data Raw = Raw { fmt :: Maybe Text,
                 longFmt :: Maybe Text,
                 raw :: Maybe Double} 
           deriving (Show, Generic)
instance FromJSON Raw {-where
    parseJSON (Object v) = Raw <$>
                           v .:? "fmt" <*>
                           v .:? "bookValue" <*>
                           v .:? "priceToBook"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero -}

newtype  Result = Result {
    result :: [DefaultKeyStats] 
}deriving (Show, Generic)

instance FromJSON Result

data Ohlc = Ohlc {
    volume :: [Double],
    close :: [Double]
}deriving (Show, Generic)

instance FromJSON Ohlc
newtype Indicator = Indicator {
    quote :: [Ohlc]
}deriving (Show, Generic)

instance FromJSON Indicator

data DefaultKeyStats = DefaultKeyStats {
                            defaultKeyStatistics :: KeyStats }
                        | Prices {
                            timestamp :: [Double],
                            indicators :: Indicator
                        }
                          deriving (Show, Generic)

instance FromJSON DefaultKeyStats where
    parseJSON (Object v) = 
        case HM.lookup "defaultKeyStatistics" v of
            Just _  -> DefaultKeyStats <$> v .: "defaultKeyStatistics" 
            Nothing -> case HM.lookup "indicators" v of
                        Just _  -> Prices <$> 
                                    v .: "timestamp" <*> 
                                    v .: "indicators" 
                        Nothing -> mzero

    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero 

data KeyStats = KeyStats {
        sharesOutstanding :: Raw,
        bookValue :: Raw,
        priceToBook ::  Raw}
    deriving (Show, Generic)

instance FromJSON KeyStats  
--instance FromJSON DefaultKeyStats

newtype Chart = Chart {
    chart :: Result 
}deriving (Show, Generic)
instance FromJSON Chart

newtype Response = Response {
    quoteSummary :: Result 
}deriving (Show, Generic)

instance FromJSON Response

robustHttp :: String -> Handler (Either HttpException C.ByteString)
robustHttp url = do
        crumbs <- getCrumb
        liftIO . try . simpleHttp $ url <> "&crumb=" <> unpack crumbs

getPricesUrl :: Symbol -> YahooConfig -> Text
getPricesUrl symbol (YahooConfig _ _ _ _ _ _ _ x y) = x <> symbol <> y 

getKeyStatUrl :: Symbol -> YahooConfig -> Text
getKeyStatUrl  symbol (YahooConfig _ _ ye ym yf yfu _ _ _)
    = ye <> yf <> yfu  <>  symbol <> ym <> "defaultKeyStatistics"
--test :: IO (Either String (KeyStats, Value))

computeReturns :: [Double] -> [Double]
computeReturns ps = 
    let fut = tail ps
        pas = init ps
        ret x y = (y - x ) / x
    in zipWith ret pas fut

getCumReturn :: Text -> Handler ( Maybe Double)
getCumReturn symbol = do 
    mchar <- getPrices symbol
    case mchar of 
        Just (Chart ch) -> return . Just . sum . computeReturns . close . 
                            head . quote . indicators . 
                            head . result $ ch
        _               -> return Nothing 

getPrices :: Text -> Handler (Maybe Chart)
getPrices  symbol = do
    url <- unpack . getPricesUrl symbol  <$> yahooSettings
    xs <- robustHttp url
    case xs of
        Right x -> do
            let mx :: Maybe Chart  = decode x
            return mx
        _ -> return Nothing



sread :: String -> Double
sread = read

