module Utils.Yahoo.Crumb where

import Import (liftIO, appSettings , Handler, getYesod)
import Settings
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Conduit
import qualified Data.Text.IO as IO
import System.Directory
import Data.Time.Clock
import Control.Monad

getCrumb :: Handler T.Text
getCrumb = do
    settings  <- appYahooConfig . appSettings <$> getYesod
    liftIO $ getCrumbIO settings

getCrumbIO :: YahooConfig ->  IO T.Text
getCrumbIO yahooSettings = do 
    let filePath = T.unpack . crumbFile $ yahooSettings
    isOld <- (>) <$> (utctDay <$> getCurrentTime) 
                 <*> (utctDay <$> getModificationTime filePath)
    when isOld $ setCrumbIO yahooSettings
    IO.readFile filePath

setCrumb :: Handler ()
setCrumb = do 
    yahooSettings <- appYahooConfig . appSettings <$> getYesod
    liftIO $ setCrumbIO yahooSettings

setCrumbIO ::  YahooConfig -> IO ()
setCrumbIO yahooSettings = do
    request <- parseRequest . T.unpack  . crumb $ yahooSettings
    cs <- do
        manager <- newManager tlsManagerSettings
        head . map (decodeUtf8 . cookie_value) . destroyCookieJar . 
            responseCookieJar <$> httpLbs request manager
    let d = T.unpack . crumbFile $ yahooSettings
    IO.writeFile d cs 

    


