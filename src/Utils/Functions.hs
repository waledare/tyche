{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module  Utils.Functions where

import Import
import Data.List (unfoldr)

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
        currentQuarter = ceiling $ ((fromIntegral m)::Double) / 3.0
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


