{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Stress where

import Import hiding  (share)
import Utils.Functions
import Utils.Yahoo.Portfolio (getPrices)
import Utils.Fred.Series (getMacro)
import Utils.TimeSeries
import qualified Prelude as P
import System.Directory (removeFile, getTemporaryDirectory)
import System.Process
import System.IO (hPutStr, openTempFile)
import System.FilePath.Posix (dropExtension)


data StressSetting = StressSetting {
    inflationRate    :: Double, 
    interestRate     :: Double,
    unemploymentRate :: Double
} deriving (Show)

stressForm :: Html -> MForm Handler (FormResult StressSetting , Widget)
stressForm extra = do 
    (inflationRes, inflationView) 
        <- mreq doubleField "" Nothing
    (interestRes, interestView) 
        <- mreq doubleField "" Nothing
    (unemploymentRes, unemploymentView) 
        <- mreq doubleField "" Nothing
    let stressWidget = do
            $(widgetFile "forms/form")
            $(widgetFile "forms/stressForm")
        stressRes = StressSetting <$> inflationRes
                                  <*> interestRes
                                  <*> unemploymentRes
    return (stressRes, stressWidget) 

positionSeries :: Position -> Handler (Symbol, TimeSeries Double)
positionSeries position = do 
    let ticker = name position
        quant  = share position 
    mChart <- getPrices ticker 
    case mChart of
        Nothing -> do 
            return (ticker, TS [] Nothing [])
        Just chart -> do 
            let seriesResult = (* (fromIntegral . snd $ quant)) <$> priceSeries' chart
            if tsNA seriesResult 
                then do 
                    error $ show quant ++ "  " ++ show (priceSeries' chart)
                    return (ticker , seriesResult)
                else return (ticker , seriesResult)

portfolioSeries :: Portfolio -> Handler (TimeSeries Double) 
portfolioSeries  (Portfolio _ ps) = do 
    tss <- mapM (fmap snd . positionSeries) ps
    if null tss
        then do 
            return $ TS [] Nothing []
        else return $ cumTS (+) tss 

writeTemp :: String -> IO P.FilePath
writeTemp str = do 
    tmpDir <- getTemporaryDirectory 
    (tmpfile, h) <- openTempFile tmpDir "temp.txt" 
    hPutStr h str
    hClose  h
    return tmpfile

scriptPath :: P.FilePath 
scriptPath = "estim.R"

runRCode :: P.FilePath -> IO String
runRCode  path = do
    let estimateFile = dropExtension path ++ ".coef"
    system $ "Rscript " ++ scriptPath ++ " " ++ path 
    res <- P.readFile $ estimateFile  
    removeFile path
    removeFile estimateFile
    return res

getStressR :: Handler Html
getStressR = do
    Entity userkey _ <- requireAuth
    userPositions' <- runDB $ 
        selectList 
            [PortfolioUserId ==. userkey][LimitTo 1]
    if null userPositions' 
        then redirect PortfolioR
        else do
        {-
            let userPositions = 
                    portfolioPortfolio . entityVal . mhead $ userPositions' 
            port <- portfolioSeries (Portfolio userkey userPositions)  
            ms <- getMacro
            let df = tsRowBind  (port : ms)
                dfStr = printFrame df
            str <- liftIO $ do
                        tempFile <- writeTemp $ unpack dfStr
                        runRCode tempFile 
            -}
            (formWidget, formEnctype) <- generateFormPost stressForm
            defaultLayout 
                    $(widgetFile "stress")
