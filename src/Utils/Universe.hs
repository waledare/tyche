{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Utils.Universe where

import Settings 
import Utils.Functions
import Import (liftIO, appSettings, Handler, getYesod) 
import Data.Text (Text, unpack, toLower, words)
import qualified Data.Text as T
import Network.HTTP.Conduit hiding (Response)
import Data.Aeson hiding (Result)
import Data.Aeson.Types (Value) 
import GHC.Generics
import Control.Monad
import qualified Data.Text.IO  as IO

type Exchange = Text

data Listing = Listing {
    nasdaqTraded :: Bool,
    symbol :: Text,
    securityName :: Text,
    exchange :: Text,
    marketCategory :: Text,
    etf :: Bool,
    lotSize :: Text,
    testIssue :: Bool,
    financialStatus :: Text,
    cqsSymbol :: Text,
    nasdaqSymbol :: Text,
    nextShares :: Text
} deriving (Show, Generic)

instance ToJSON Listing

data UniverseSelector = ADR | Rest | All deriving (Show)

getUniverse' :: UniverseSelector -> Handler [Listing]
getUniverse' selector  =  do
    let assembler [n, s, se, ex, ma, et, lo, tes, fin, cqs, nas, nex] =
            Listing ( truth n) s se ex ma (truth et)  lo (truth tes) fin cqs nas nex 
        assembler a = error . show $ a  
        truth n = n == "Y"  
        parser = map assembler . filter ((12 == ) . length ) . map (T.split (== '|')) .  tail . T.lines   
        adrs = map T.toLower ["ADR", "Depositary", "Depository"] :: [Text]
        adr listing = 
            any (\x -> x `elem` (map T.toLower . T.words . securityName $ listing)) adrs
        f x = case selector of
                ADR    -> adr x 
                Rest   -> (not . etf $ x) && ( not . testIssue $ x) && (not . adr $ x)
                All    -> True  
    let nasdaqSettings = appNasdaqConfig . appSettings <$> getYesod
    NasdaqConfig n o <- nasdaqSettings 
    mprint (n, o) 
    liftIO $
        filter f . parser . T.filter (/= '\r')  <$> liftM2 (<>) (IO.readFile n) (IO.readFile o)

getUniverse = getUniverse' Rest

getADR = getUniverse' ADR





    
