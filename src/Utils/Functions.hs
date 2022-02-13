{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module  Utils.Functions where
import Yesod.Form.Fields
import Yesod.Core
import Yesod.Form.Types
import Yesod.Form.Functions
import Data.Text.Read
import Debug.Trace


import Data.Text (unpack, pack, Text)
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Yesod.Core
import Utils.Types
import Data.List (group, sort, unfoldr)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock
import Data.Time.Calendar(toGregorian)
import Control.Monad.IO.Class
import Control.Exception
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as C 
import Debug.Trace
import Yesod.Form.Fields
import Yesod.Form.Types
debug :: String -> a -> a
debug msg a = 
    let open  = "----------DEBUG-----------\n\n"
        close = "\n\n----------DEBUG-----------\n"
    in  trace (open ++ msg ++ close) a


prependZero :: Text -> Text
prependZero t0 = if T.null t1
                 then t1
                 else if T.head t1 == '.'
                      then '0' `T.cons` t1
                      else if "-." `T.isPrefixOf` t1
                           then "-0." `T.append` (T.drop 2 t1)
                           else t1

  where t1 = T.dropWhile (==' ') t0

{-
rangeField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
rangeField  = Field
    { fieldParse = parseHelper Right
        , fieldView = \theId name attrs val isReq ->
            [whamlet|
                $newline never
                    <input id="#{theId}" name="#{name}" *{attrs} type="range">
            |]
        , fieldEnctype = UrlEncoded
    }
-}
rangeField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Double
rangeField  = Field
    { fieldParse = parseHelper $ \s ->
        case Data.Text.Read.double (prependZero s) of
            Right (a, "") -> Right a
            _ -> Left $ MsgInvalidNumber s

    , fieldView = \theId name attrs val isReq -> [whamlet|
$newline never
                    <input id="#{theId}" name="#{name}" *{attrs} type="range">
|]
    , fieldEnctype = UrlEncoded
    }
  where showVal = either id (pack . show)



getHttp :: String -> IO (Either IOError C.ByteString) 
getHttp = try . simpleHttp

merge :: Ord a => [[a]] -> [a]
merge = sort . map mhead . group . concat 

type Symbol = Text

name :: Position -> Symbol 
name = symbol . listing 

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

debugM str = debug str (pure ()) 
mprint :: (MonadIO m, Show a) => a -> m ()
mprint str = liftIO $ do
    print "-------------- START ---------------------"
    print str
    print "--------------- END -----------------"

mwrite :: (MonadIO m) => String -> m ()
mwrite str = liftIO $ do
    appendFile "debug" "-------------- START ---------------------"
    appendFile "debug" str
    appendFile "debug"  "--------------- END -----------------"


groupBy :: Show a => (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy p' (x':xs') = (x' : ys') : zs'
  where
    (ys',zs') = go p' x' xs'
    go p z (x:xs)
      | p z x = trace (show z ++ "\t" ++ show x ++ "\n\n\n\n")  (x : ys, zs)
      | otherwise =  ([], (x : ys) : zs)
      where (ys,zs) = go p x xs
    go _ _ [] = ([], [])


sgn :: (Ord a, Num a) => a -> a
sgn x = if x > 0 then 1 else -1

mlog :: Double -> Double 
mlog x | x  > 0 = -log x
       | x == 0 = 15
       | otherwise = 15 +   (abs . log . abs $ x )
