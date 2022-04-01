{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Positions where

import Import
import Text.Read (read)

getPositionsR :: Handler Value
getPositionsR = do
    Entity userkey _ <- requireAuth
    mps <- lookupSession "positions" 
    let vs :: [Position]
        vs = case mps of
                Just ps -> read $ unpack ps
                Nothing -> []
    {-
    vs <- map (portfolioPortfolio . entityVal) <$> 
            runDB (selectList [PortfolioUserId ==. userkey][]) 
    -}
    return $ toJSON vs
