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


merge :: Ord a => [[a]] -> [a]
merge = sort . map mhead . group . concat 

type Symbol = Text

name :: Position -> Symbol 
name = symbol . listing 

tsValues :: TimeSeries a -> [Maybe a]
tsValues (TS xs) = map snd  xs

tsDates :: TimeSeries a -> [Day]
tsDates (TS xs) = map fst xs

scalarMultTS :: Num a => TimeSeries a -> a  -> TimeSeries a
scalarMultTS ts x = TS $ zip (tsDates ts) ( map ( fmap  (* x)) $ tsValues ts) 

binaryOpTS :: (Eq a, Num a) =>  (a -> a -> a) -> TimeSeries a -> TimeSeries a -> TimeSeries a
binaryOpTS op (TS ats)  (TS bts) = 
    let groups = group . concat $ [ats, bts]
        f :: (Eq a, Num a) => (a->a->a) -> [(Day, Maybe a)] -> (Day, Maybe a)
        f op xs | length xs == 1 = 
                    let [(dt , _ )] = xs
                    in  (dt, Nothing)
        f op xs | length xs == 2 = 
                let [ (day, av) ,(_, bv)] = xs
                in  (day, (op) <$> av <*> bv) 
                | otherwise  =  (fromGregorian 1900 1 1, Nothing)
    in TS $ map (f op)  groups 

addTS = binaryOpTS (+)
multTS = binaryOpTS (*)

cumTS f xs | length xs == 0 = TS []
cumTS f xs | length xs == 1 = mhead xs
           | otherwise = 
               let  g [] t     = t
                    g (x:xs) t = g xs (binaryOpTS f t x)
               in   g (mtail xs) (mhead xs) 


    
priceSeries' :: Chart -> TimeSeries Double
priceSeries' c =  
    let vals = close . mhead . quote . indicators . mhead . result . chart $ c
        ds   = map (utctDay . secondsToUTC) . 
                    timestamp . mhead . result . chart $ c
    in  TS (zip ds vals)

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


