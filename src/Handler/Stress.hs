{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Stress where

import Import
import Utils.Functions

getStressR :: Handler Html
getStressR = do
    Entity userkey _ <- requireAuth
    {-- 1. Get current portfolio weights
     -- 2. Obtain historical data for all assets in portfolio
     -- 3. Compute the porfolio returns
     --}
    userPortfolio <- runDB $ 
        portfolioPortfolio . entityVal . mhead <$> selectList 
            [PortfolioUserId ==. userkey][LimitTo 1]
    defaultLayout $ do
        $(widgetFile "stress")
