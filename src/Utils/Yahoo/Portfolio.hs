-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DeriveGeneric #-}
module Utils.Yahoo.Portfolio where
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Utils.Functions (mtail, mhead, maybeHead, mlast,secondsToUTC)
import Database.Persist.TH
import Import (loadYamlSettings, 
                PriceStats,
                Handler, 
                sort, 
                useEnv, 
                liftIO ,  
                appSettings, 
                getYesod)
import Import hiding (init, head, tail, try, unpack) 
import Utils.Yahoo.Crumb
import Settings  
import Prelude ( head, tail, init, read)
import Data.Text (  Text, 
                    unpack)
import Network.HTTP.Conduit hiding (Response)
import Data.Aeson hiding (Result)
import Data.Maybe (fromJust)
import GHC.Generics
import Control.Monad
import Data.Time.Clock ( diffUTCTime, nominalDay)
import Control.Exception (try)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.HashMap.Strict as HM

yahooSettings :: Handler YahooConfig
yahooSettings = appYahooConfig . appSettings <$> getYesod


yahooSettingsIO :: IO YahooConfig 
yahooSettingsIO = do 
    YahooConfig' yahoo <- loadYamlSettings["config/settings.yml"] [] useEnv
    return yahoo 

type Module = Text
type Symbol = Text

robustHttpIO :: YahooConfig -> String -> IO (Either HttpException C.ByteString)
robustHttpIO yahooSettings url = do
        crumbs <- getCrumbIO yahooSettings
        try . simpleHttp $ url <> "&crumb=" <> unpack crumbs


robustHttp :: String -> Handler (Either HttpException C.ByteString)
robustHttp url =  yahooSettings >>= liftIO . flip robustHttpIO url

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


type Range = String

getPricesIO :: Range -> YahooConfig ->  Text -> IO (Maybe Chart)
getPricesIO range yahooSettings symbol = do 
    let url = unpack . getPricesUrl symbol $ yahooSettings
        hurl = takeWhile (/= '5') url
        turl = mtail . mtail . dropWhile (/= '5') $ url
    either (const Nothing) decode 
        <$> robustHttpIO yahooSettings (if null range then url else hurl <> range<> turl)
    
getPrices :: Text -> Handler (Maybe Chart)
getPrices  symbol = do 
    let dated :: PriceStats ->  IO Bool
        dated ps = do 
            now <- getCurrentTime
            let end =  priceStatsEnd ps 
                (_, nowMonth, nowYear)  = toGregorian . utctDay $ now 
                (_, endMonth, endYear)  = toGregorian . utctDay $ end 
            return (if nowYear == endYear
                        then nowMonth - endMonth > 1
                        else diffUTCTime now end > 31 * nominalDay )
    stat <- runDB $ map entityVal <$> 
                    selectList [PriceStatsSymbol ==. symbol] [LimitTo 1]
    dataStatus <-  
           if null stat
               then return Unavailable
               else do
                    itis <- liftIO . dated . fromJust . maybeHead $ stat 
                    if itis
                        then return OutDated 
                        else return UpToDate
    case dataStatus of
        Unavailable -> do 
            mChart <-yahooSettings >>= liftIO . flip (getPricesIO "50") symbol 
            case mChart of
                Just charts    -> do
                    let mktDatas = result . chart $ charts 
                    if  null mktDatas
                        then return Nothing
                        else do
                                let mkt = mhead mktDatas 
                                    ts  = sort $ map secondsToUTC $ 
                                                    timestamp mkt
                                    end = mlast ts
                                    start = mhead ts
                                runDB $ do
                                    insert (Prices symbol mkt)
                                    insert (PriceStats symbol start end)
                                return . Just $ charts 
                Nothing    -> return Nothing
        OutDated -> do
            now <- liftIO getCurrentTime 
            let end = priceStatsEnd . fromJust . maybeHead $ stat
                (_, nowMonth, nowYear)  = toGregorian . utctDay $ now 
                (_, endMonth, endYear)  = toGregorian . utctDay $ end 
                months = (nowYear - endYear) * 12 + nowMonth - endMonth + 1 
                mm :: String
                mm = show months  
            mChart <-  yahooSettings >>= liftIO . flip (getPricesIO mm) symbol 
            case mChart of
                Just charts -> do
                    let mktDatas = result . chart $ charts 
                    if  null mktDatas
                        then return Nothing
                        else do 
                                let mkt = mhead mktDatas 
                                    newts = timestamp mkt
                                    newcs = close . mhead . quote . 
                                                indicators $ mkt
                                ps <- runDB $ mhead . map entityVal <$> 
                                        selectList [PricesSymbol ==. symbol]
                                                   [LimitTo 1]
                                let oldcs = close . mhead . quote . 
                                                indicators . pricesPrices $ ps
                                    ots   = timestamp . pricesPrices $ ps 
                                    npairs = filter ( (`notElem` ots) . fst ) 
                                                    $ zip newts newcs
                                    uts = map fst npairs ++ ots
                                    ucs = map snd npairs ++ oldcs
                                    mcs = MarketData uts (Indicator [Ohlc ucs])
                                runDB $ updateWhere  [PricesSymbol ==. symbol]
                                                     [PricesPrices =. mcs]
                                return . Just . Chart . Result $ [mcs]
                Nothing -> return Nothing
            return Nothing 
        UpToDate -> do
            mkts <- runDB $ map entityVal <$> 
                            selectList [PricesSymbol ==. symbol][LimitTo 1]
            return . Just . Chart . Result . map pricesPrices $  mkts

sread :: String -> Double
sread = read

