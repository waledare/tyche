{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Widget.Horizon  where
import Import

forecastForm :: Html -> MForm Handler (FormResult Integer, Widget)
forecastForm extra = do 
    -- This should go into a utility file
    let settings name  str = (fromString str) {
            fsName = Just name
        }
    (eaRes, eaView) <- mreq intField (settings "horizonLength" "Equity allocation") Nothing
    let eaWidget = 
            [whamlet|
                #{extra}
                <div>
                    <div>
                        Horizon
                    <div>
                        ^{fvInput eaView}
            |]
    return (eaRes , eaWidget)


forecastWidget :: Route App -> Text -> Handler Widget 
forecastWidget actionR method = do
    (widget, encType) <- generateFormPost forecastForm
    let newidget = 
            [whamlet|
                <form name="forecast" action=@{actionR} method=#{method} enctype=#{encType}>
                    ^{widget}
                    <input type=submit>
            |]
    return newidget 

 

