{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Fred.Series where

import Settings
import GHC.Generics
import Utils.Functions hiding (Symbol) 
import Utils.Types
import Utils.TimeSeries hiding (observations)
import Data.Aeson.Types
import Data.Aeson(decode, eitherDecode)
import Data.Time.Calendar
import Data.Text.Encoding
import Data.Text( Text)
import Import 
import Prelude (read)
import Data.Maybe (fromJust)


data FredSeries a  = FS {
    units :: Maybe Text,
    observations :: [Observation a]
} deriving (Show, Generic, Read, Eq)

instance FromJSON a => FromJSON (FredSeries a )

instance ToJSON a => ToJSON (FredSeries a )

instance Functor FredSeries where
    fmap f (FS au obs) = 
        let g :: (a -> b) -> Observation a -> Observation b
            g f (Observation s e d v) = Observation s e d (f <$> v)
        in  FS au (map (g f) obs)



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

getSeriesIO :: FredConfig -> Text -> IO (Either String (FredSeries Double))
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
            eseries <- fredSettings >>= liftIO . flip getSeriesIO symbol 
            case eseries of
                Right series'    -> do
                    let series = TS [symbol] Nothing (observations series')
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
            eseries <-fredSettings >>= liftIO . flip getSeriesIO symbol 
            case eseries of
                Right series'    -> do 
                    let series = TS [symbol] Nothing (observations series')
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

getMacro = do
    mint <- getInterestRate
    minf <- getInflation
    mun  <- getUnemployment
    ggdp <- getGdp
    return $ map fromJust $ filter (isJust) [minf, mint, mun, ggdp] 

