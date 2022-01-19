{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Stress where

import Import hiding  (share)
import Utils.Functions
import Utils.Yahoo.Portfolio (getPrices)
import Utils.TimeSeries


positionSeries :: Position -> Handler (Symbol, TimeSeries Double)
positionSeries position = do 
    let ticker = name position
        quant  = share position 
    mChart <- getPrices ticker 
    case mChart of
        Nothing -> return (ticker, TS Nothing [])
        Just chart -> return (ticker , 
            fmap ( * (fromIntegral . snd $ quant)) $ priceSeries' chart )

portfolioSeries :: Portfolio -> Handler (TimeSeries Double) 
portfolioSeries  (Portfolio _ ps) = do 
    tss <- mapM (fmap snd . positionSeries) ps
    if null tss
        then return $ TS Nothing []
        else return $ cumTS (+) tss 

getStressR :: Handler Html
getStressR = do
    Entity userkey _ <- requireAuth
    {-- 1. Get current portfolio weights
     -- 2. Obtain historical data for all assets in portfolio
     -- 3. Compute the porfolio returns
     --}
    userPositions <- runDB $ 
        portfolioPortfolio . entityVal . mhead <$> selectList 
            [PortfolioUserId ==. userkey][LimitTo 1]
    let userSymbols = map (symbol . listing) userPositions  
    rmcharts <- mapM getPrices userSymbols
    defaultLayout 
        $(widgetFile "stress")
