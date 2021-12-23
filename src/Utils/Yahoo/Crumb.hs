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
    filePath <- T.unpack . crumbFile . appYahooConfig . appSettings <$> getYesod
    isOld <- liftIO $ 
                (>) <$> (utctDay <$> getCurrentTime) 
                     <*> (utctDay <$> getModificationTime filePath)
    when isOld setCrumb
    liftIO $ IO.readFile filePath

setCrumb :: Handler ()
setCrumb = do
    let yahooSettings = appYahooConfig . appSettings <$> getYesod
    request <- liftIO .  parseRequest  =<< (T.unpack  . crumb <$> yahooSettings)
    cs <- liftIO $ do
            manager <- newManager tlsManagerSettings
            head . map (decodeUtf8 . cookie_value) . destroyCookieJar . 
                responseCookieJar <$> httpLbs request manager
    d <- T.unpack . crumbFile <$> yahooSettings
    liftIO $ IO.writeFile d cs 

    


