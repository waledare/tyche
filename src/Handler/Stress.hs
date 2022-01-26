{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Stress where

import Import hiding  (share)
import Utils.Functions
import Utils.Yahoo.Portfolio (getPrices)
import Utils.Fred.Series (getMacro)
import Utils.TimeSeries
import qualified Prelude as P

positionSeries :: Position -> Handler (Symbol, TimeSeries Double)
positionSeries position = do 
    let ticker = name position
        quant  = share position 
    mChart <- getPrices ticker 
    case mChart of
        Nothing -> return (ticker, TS [] Nothing [])
        Just chart -> return (ticker , 
            (* (fromIntegral . snd $ quant)) <$> priceSeries' chart )

portfolioSeries :: Portfolio -> Handler (TimeSeries Double) 
portfolioSeries  (Portfolio _ ps) = do 
    tss <- mapM (fmap snd . positionSeries) ps
    if null tss
        then return $ TS [] Nothing []
        else return $ cumTS (+) tss 

getStressR :: Handler Html
getStressR = do
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
            let df = tsRowBind  (port : ms)
                dfStr = printFrame df
            liftIO $ P.writeFile "dfStr" $ unpack dfStr
            defaultLayout 
                    $(widgetFile "stress")
