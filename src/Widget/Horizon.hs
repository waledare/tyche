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
            , fsId   = Just "horizonInput"
            , fsAttrs = [("min", "1"), ("max", "120")]
        }
    (eaRes, eaView) <- mreq intField (settings "horizonLength" "Equity allocation") Nothing
    let widget = do
                    $(widgetFile "forms/form")
                    [whamlet|
                        #{extra}
                        <div>
                            <div>
                                Number of quarters in the future
                            <div>
                                ^{fvInput eaView}
                    |]
    return (eaRes , widget)


 

