{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Portfolio where

import Utils.Universe
import Utils.Functions (mprint, maybeHead)
import Import
import Data.List ((!!))

tomSelectJs :: Text
tomSelectJs = "https://cdn.jsdelivr.net/npm/tom-select@2.0.0/dist/js/tom-select.complete.min.js"

tomSelectCss :: Text
tomSelectCss = "https://cdn.jsdelivr.net/npm/tom-select@2.0.0/dist/css/tom-select.css"

symbolForm :: Html -> MForm Handler (FormResult (Text, Integer), Widget)
symbolForm extra = do 
    let settings name  str = (fromString str) {
            fsName = Just name
            , fsId   = Just "symbolInput"
        }
        quantitySettings str = (fromString str) { 
            fsTooltip = Just (fromString "Quantity"),  
            fsId     = Just "quantityInput", 
            fsAttrs  =  [("min", "0")]
        }
    let emptyList :: [(Text, Text)]
        emptyList = empty
        symbolPairs = optionsPairs emptyList
    (symbolRes, symbolView) 
        <- mreq (selectField symbolPairs) 
                (settings "symbolName" "Asset symbol") Nothing
    (quantityRes, quantityView) 
        <- mreq intField (quantitySettings "Quantity") Nothing
    let symbolWidget = do
            $(widgetFile "forms/form")
            $(widgetFile "forms/symbolForm")
    return ((,) <$> symbolRes <*> quantityRes , symbolWidget)

getPortfolioR :: Handler Html
getPortfolioR = do
    Entity _ _ <- requireAuth
    (formWidget, formEnctype) <- generateFormPost symbolForm
    defaultLayout $ do
        symbolId <- newIdent 
        -- addScriptRemote tomSelectJs 
        -- addStylesheetRemote tomSelectCss
        $(widgetFile "portfolio")

putPortfolioR :: Handler Value
putPortfolioR = do 
    Entity userkey _ <- requireAuth
    selections <- requireCheckJsonBody :: Handler [Position]
    runDB $ upsert  (Portfolio userkey selections) 
                    [PortfolioPortfolio =. selections]
    return $ toJSON ( "Success" :: Text) 
