{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Diagnosis where

import Import
import Utils.Functions
import Utils.Yahoo.Portfolio
import Utils.TimeSeries
import Utils.Fred.Series hiding (getSP)
import Handler.Stress (runRCode, assessHealth, writeTemp, portfolioSeries)

postDiagnosisR :: Handler Value
postDiagnosisR = do 
    Entity userkey _ <- requireAuth
    userPositions' <- runDB $ 
        selectList 
        [PortfolioUserId ==. userkey][LimitTo 1]
    if null userPositions'
        then redirect PortfolioR
        else do
            let userPositions = 
                    portfolioPortfolio . entityVal . mhead $ userPositions' 
            port <- portfolioSeries (Portfolio userkey userPositions)  
            ms <- getMacro
            sp <- fmap priceSeries' <$> getSP
            let df = case sp of 
                        Just spp -> tsRowBind  (port : (ms++ [spp]))
                        Nothing  -> tsRowBind  (port : ms)
                dfStr = printFrame df
            str <- liftIO $ do
                        tempFile <- writeTemp $ unpack dfStr
                        fmap robustness <$> runRCode tempFile
            return $ toJSON str

getDiagnosisR :: Handler Html
getDiagnosisR = do
    defaultLayout $ do 
        addScript $ StaticR jss_raphael_js
        addScript $ StaticR jss_justgage_js
        $(widgetFile "diagnosis")


-- TODO 
-- The key is value. That value is emotional. People are emotional 
-- about the health of their retirements
-- 1. The first thing is a general assessment
-- 2. You then have the option to dig in 

robustness :: ROut -> Double 
robustness (ROut bs ss rs _) = 
    let foo x = 100 /(1 + exp (-1*x))
        [_, _, _, _, cm] = bs
    in  foo $ rs + mlog cm
