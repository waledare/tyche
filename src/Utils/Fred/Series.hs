{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Fred.Series where

import Settings
import GHC.Generics
import Utils.Functions hiding (Symbol) 
import Utils.Types
import Utils.TimeSeries
import Data.Aeson.Types
import Data.Aeson(decode, eitherDecode)
import Data.Time.Calendar
import Data.Text.Encoding
import Data.Text( Text)
import Import 
import Prelude (read)
import Data.Maybe (fromJust)

inflation :: Text
inflation = "CPIAUCSL"

gdp :: Text
gdp = "GDPC1"

unemployment :: Text
unemployment = "UNRATE"

interestrate :: Text
interestrate = "FEDFUNDS"

fredSettingsIO :: IO FredConfig 
fredSettingsIO = do 
    FredConfig' fred <- loadYamlSettings["config/settings.yml"] [] useEnv
    return fred 

type Symbol = Text

mkUrl :: FredConfig -> Symbol -> String
mkUrl (FredConfig endpt api ft) symbol = 
    let url = endpt <> "series_id=" 
                    <> symbol <> "&api_key=" 
                    <> api <> "&file_type=" <> ft
    in  unpack url 

getSeriesIO :: FredConfig -> Text -> IO (Either String (TimeSeries Double))
getSeriesIO conf symbol = do 
    let url = mkUrl conf symbol 
    ebytes <- getHttp url
    case ebytes of 
        Left e  -> return $ Left  $ show  e
        Right x -> return ( fmap read <$> eitherDecode x)  

fredSettings :: Handler FredConfig 
fredSettings = liftIO fredSettingsIO ----
    
getSeries :: Text -> Handler (Maybe (TimeSeries Double))
getSeries  symbol = do 
    let dated :: MacroStats ->  IO Bool
        dated ps = do 
            now <- getCurrentTime
            let end =  macroStatsEnd ps 
                (_, nowMonth, nowYear)  = toGregorian . utctDay $ now 
                (_, endMonth, endYear)  = toGregorian end 
            return (if nowYear == endYear
                        then nowMonth - endMonth > 1
                        else (nowYear - endYear) * 12 + (nowMonth - endMonth) > 1 )
    stat <- runDB $ map entityVal <$> 
                    selectList [MacroStatsSymbol ==. symbol] [LimitTo 1]
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
            mprint $ (toText Unavailable) <> " " <> symbol
            eseries <- fredSettings >>= liftIO . flip getSeriesIO symbol 
            case eseries of
                Right series    -> 
                    if  tsNull series
                        then return Nothing
                        else do
                                let ss  = sort $ tsDates series
                                    end = mlast ss
                                    start = mhead ss
                                runDB $ do
                                    insert (Macro symbol series)
                                    insert (MacroStats symbol start end)
                                return . Just $ series 
                Left _    -> return Nothing
        OutDated -> do
            mprint $ (toText OutDated) <> " " <> symbol
            eseries <-fredSettings >>= liftIO . flip getSeriesIO symbol 
            case eseries of
                Right series    -> 
                    if  tsNull series
                        then return Nothing
                        else do
                                let ts  = sort $ tsDates series
                                    end = mlast ts
                                    start = mhead ts
                                runDB $ do
                                    updateWhere  [MacroSymbol ==. symbol]
                                                     [MacroSeries =. series]
                                    updateWhere [MacroStatsSymbol ==. symbol]
                                                    [MacroStatsEnd =. end]
                                return . Just $ series 
                Left _    -> return Nothing
        UpToDate -> do
            mprint $ (toText OutDated) <> " " <> symbol
            mkts <- runDB $ map entityVal <$> 
                            selectList [MacroSymbol ==. symbol][LimitTo 1]
            return . Just $  macroSeries . mhead $ mkts

getInflation :: Handler (Maybe (TimeSeries Double))
getInflation = getSeries inflation

getInterestRate :: Handler (Maybe (TimeSeries Double))
getInterestRate = getSeries interestrate

getUnemployment :: Handler (Maybe (TimeSeries Double))
getUnemployment = getSeries unemployment

getGdp :: Handler (Maybe (TimeSeries Double))
getGdp = getSeries gdp
