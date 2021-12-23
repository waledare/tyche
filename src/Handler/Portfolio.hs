{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Portfolio where

import Import
import Utils.Universe
import Data.List ((!!))

tomSelectJs :: Text
tomSelectJs = "https://cdn.jsdelivr.net/npm/tom-select@2.0.0/dist/js/tom-select.complete.min.js"

tomSelectCss :: Text
tomSelectCss = "https://cdn.jsdelivr.net/npm/tom-select@2.0.0/dist/css/tom-select.css"

symbolForm :: Html -> MForm Handler (FormResult Text, Widget)
symbolForm extra = do 
    let settings name  str = (fromString str) {
            fsName = Just name
            , fsId   = Just "symbolInput"
        }
    symbols  <- map symbol <$> lift getUniverse
    let symbolPairs = optionsPairs (zip symbols symbols)
    (symbolRes, symbolView) 
        <- mreq (selectField symbolPairs) (settings "symbolName" "Asset symbol") Nothing
    let symbolWidget = do
            $(widgetFile "forms/form")
            $(widgetFile "forms/symbolForm")
    return (symbolRes , symbolWidget)

getPortfolioR :: Handler Html
getPortfolioR = do
    (_, user) <- requireAuthPair
    (formWidget, formEnctype) <- generateFormPost symbolForm
    defaultLayout $ do
        symbolId <- newIdent 
        addScriptRemote tomSelectJs 
        addStylesheetRemote tomSelectCss
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "portfolio")

putPortfolioR :: Handler Value
putPortfolioR = do 
    symbolName <- (\x -> x -1 )  <$> requireCheckJsonBody :: Handler Int
    let symbolNamePair x = (symbol x, takeWhile (/= '-' ) . securityName $ x) 
    listing <- ( !! symbolName) . map symbolNamePair <$> getUniverse 
    return $ toJSON listing 
