{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils.TimeSeries  where 
import Database.Persist.TH
import GHC.Generics
import Data.Aeson hiding (Series, Result)
import Prelude
import Utils.Functions
import Utils.Types
import Data.List (sort, group)
import Data.Time.Clock (utctDay, UTCTime)
import Data.Time.Calendar (fromGregorian, Day)
import Data.Text (Text)
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as C 

type Series = TimeSeries Double

data TimeSeries a  = TS {
    units :: Maybe String,
    observations :: [Observation a]
} deriving (Show, Generic, Read, Eq)

instance FromJSON a => FromJSON (TimeSeries a )
instance ToJSON a => ToJSON (TimeSeries a )

instance Functor TimeSeries where
    fmap f (TS au obs) = 
        let g :: (a -> b) -> Observation a -> Observation b
            g f (Observation s e d v) = Observation s e d (f <$> v)
        in  TS au (map (g f) obs)

data Observation a = Observation {
    realtime_start :: Maybe Day,
    realtime_end   :: Maybe Day,
    date    :: Day,
    value   :: Maybe a
} deriving (Show, Generic, Read, Eq)

instance FromJSON a => FromJSON (Observation a) 

instance ToJSON a => ToJSON (Observation a) 

instance Eq a => Ord (Observation a) where
    compare a b = compare (date a) (date b)

derivePersistField "Series"

tsNull (TS _ os) = null os 

tsValues :: TimeSeries a -> [Maybe a]
tsValues  = map value . observations

tsDates :: TimeSeries a -> [Day]
tsDates  = map date . observations

tsStart :: TimeSeries a -> [Maybe Day]
tsStart  = map realtime_start . observations

tsEnd :: TimeSeries a -> [Maybe Day]
tsEnd  = map realtime_end . observations

scalarMultTS :: Num a => TimeSeries a -> a  -> TimeSeries a
scalarMultTS ts x = fmap (*x) ts 

nullOb = Observation Nothing Nothing (fromGregorian 1900 1 1)  Nothing

binaryOpTS :: (Eq a, Num a) =>  (a -> a -> a) -> TimeSeries a -> TimeSeries a -> TimeSeries a
binaryOpTS op (TS au ats)  (TS bu bts) = 
    let groups = group . concat $ [ats, bts]
        f :: (Eq a, Num a) => (a->a->a) -> [Observation a] -> Observation a
        f op xs | length xs == 1 = 
                    let [Observation s e dt _] = xs
                    in  Observation s e dt Nothing
        f op xs | length xs == 2 = 
                let [Observation s e dt av, Observation _ _ _ bv] = xs
                in  Observation s e dt (op <$> av <*> bv) 
                | otherwise  = nullOb 
    in TS  ((<>) <$> au <*> bu)  $ map (f op)  groups 

addTS = binaryOpTS (+)
multTS = binaryOpTS (*)

cumTS :: (Eq a,Num a)  => (a -> a ->a) -> [TimeSeries a] -> TimeSeries a
cumTS f xs | null xs  = TS Nothing []
cumTS f xs | length xs == 1 = mhead xs
           | otherwise = 
               let  g :: (Eq a,Num a)  => (a -> a -> a) -> [TimeSeries a] -> TimeSeries a -> TimeSeries a
                    g f [] t     = t
                    g f (x:xs) t = g f xs (binaryOpTS f t x)
               in   g f (mtail xs) (mhead xs) 

priceSeries' :: Chart -> TimeSeries Double
priceSeries' c =  
    let vals = close . mhead . quote . indicators . mhead . result . chart $ c
        ds   = map (utctDay . secondsToUTC) . 
                    timestamp . mhead . result . chart $ c
    in  TS (Just "lin") (zipWith (Observation Nothing Nothing)  ds vals)

tsRowBind :: Eq a => [TimeSeries a] -> TimeSeries [Maybe a]
tsRowBind ts = 
    let dates  = merge . map tsDates $ ts
        ncol   = length ts
        nts    = map (fillMissing dates) ts
        allObs = concat . map observations $ nts
        f dt   = 
            let oss  = map observations nts
                g os = value . mhead . filter ((== dt) . date) $ os
                vs   = map g oss
            in Observation Nothing Nothing dt (Just vs)
    in TS Nothing $  map f dates 

fillMissing :: Eq a => [Day] -> TimeSeries a -> TimeSeries a
fillMissing ds  ts = 
    let fullds = merge [ds, tsDates ts]
        notPresent = filter ( `notElem` (tsDates ts)  ) fullds
        f dt = nullOb {date = dt}
    in  TS (units ts) $ sort $ map f notPresent <> observations ts

type DataFrame a = TimeSeries [Maybe a]

