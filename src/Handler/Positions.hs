{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Positions where

import Import

getPositionsR :: Handler Value
getPositionsR = do
    Entity userkey _ <- requireAuth
    vs <- map (portfolioPortfolio . entityVal) <$> 
            runDB (selectList [PortfolioUserId ==. userkey][]) 
    return $ toJSON vs
