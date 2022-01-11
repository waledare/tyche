{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Symbols where

import Import
import Utils.Universe (getUniverse)

getSymbolsR :: Handler Value
getSymbolsR = do 
    (_, _) <- requireAuthPair
    toJSONList <$> getUniverse
