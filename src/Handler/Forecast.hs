{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Forecast where

import Import
import Widget.Horizon (forecastForm)
import Utils.Analysis
import Utils.Functions
import qualified Prelude as P

chartjs :: Text
chartjs = "https://cdn.jsdelivr.net/npm/chart.js"

getForecastR :: Handler Html
getForecastR = do
    (formWidget, formEnctype) <- generateFormPost forecastForm
    defaultLayout $ do
        addScriptRemote chartjs 
        $(widgetFile "forecast")

postForecastR :: Handler Value
postForecastR = do
    n <- requireJsonBody :: Handler Integer
    staticDir <- appStaticDir . appSettings <$> getYesod
    cpi <- liftIO $ P.readFile $ staticDir </> "data/cpi.txt"
    let cpiData = take (fromIntegral n) . mtail . P.lines $ cpi
    labels <- liftIO $ getQuarters (min n $ fromIntegral $ length cpiData)
    return $ toJSON (labels, cpiData) 



    
        
