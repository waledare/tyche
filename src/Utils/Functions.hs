{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module  Utils.Functions where

import Prelude (read, Maybe(..))
import Data.Maybe (fromJust)
import Import
import Utils.Types
import Data.List (unfoldr)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as C 



getHttp :: String -> IO (Either IOError C.ByteString) 
getHttp = try . simpleHttp

merge :: Ord a => [[a]] -> [a]
merge = sort . map mhead . group . concat 

type Symbol = Text


name :: Position -> Symbol 
name = symbol . listing 

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

share :: Position -> (Symbol, Integer)
share  p = ( name  p, quantity p) 

secondsToUTC = posixSecondsToUTCTime . fromInteger

mhead = fromJust . maybeHead

mlast = mhead . reverse

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

toText :: Show a => a -> Text
toText = pack . show 

fromText :: Read b => Text -> b
fromText = read . unpack 

mtail :: [a] ->  [a]
mtail (_:xs) = xs
mtail []   = []



getMonthYear :: UTCTime -> (Integer, Int)
getMonthYear t =
    let (y, m, _) = toGregorian . utctDay $ t
    in (y, m)

getQuarters :: Integer -> IO [String]
getQuarters n = do
    (y,m) <- getMonthYear <$> getCurrentTime
    let currentQuarter :: Integer
        currentQuarter = ceiling $ (fromIntegral m::Double) / 3.0
        nextQuarters cQ =  1 + mod cQ 4
        qys = unfoldr (\(i, cQ, yr) -> 
                        if i > n 
                            then Nothing 
                            else 
                                let nextQ = nextQuarters cQ
                                    nextYr = if nextQ == 1
                                                then yr + 1
                                                else yr
                                in  Just ( (nextQ, nextYr), (i+1, nextQ, nextYr ))) (1, currentQuarter,y)
    return $ map (\(q,yy) -> "Q"<> show q <> " " <> show yy) qys

mprint :: (MonadIO m, Show a) => a -> m ()
mprint str = liftIO $ do
    print "-------------- START ---------------------"
    print str
    print "--------------- END -----------------"


