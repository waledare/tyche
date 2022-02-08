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

stressForm :: Html -> MForm Handler (FormResult Shocks  , Widget)
stressForm extra = do 
    let inflationSettings = 
            (fromString "Inflation") {fsId = Just "inflationNub", 
                                      fsAttrs = infAttrs}
        interestSettings = 
            (fromString "Interest") {fsId = Just "interestNub",
                                      fsAttrs = intAttrs}
        unemploymentSettings = 
            (fromString "Unemployment") {fsId = Just "unemploymentNub",
                                      fsAttrs = uneAttrs}
        infAttrs = [("min", "-5"), ("max", "5"), ("step", "0.01")]
        intAttrs = [("min", "-5"), ("max", "25"), ("step", "0.01")]
        uneAttrs = [("min", "0"), ("max", "25"), ("step", "0.01")]
    (inflationRes, inflationView) 
        <- mreq rangeField inflationSettings Nothing
    (interestRes, interestView) 
        <- mreq rangeField interestSettings Nothing
    (unemploymentRes, unemploymentView) 
        <- mreq rangeField unemploymentSettings Nothing
    let stressWidget = do
            $(widgetFile "forms/form")
            $(widgetFile "forms/stressForm")
        stressRes = Shocks <$> inflationRes
                                  <*> interestRes
                                  <*> unemploymentRes
    return (stressRes, stressWidget) 

positionSeries :: Position -> Handler (Symbol, TimeSeries Double)
positionSeries position = do 
    let ticker = name position
        quant  = share position 
    mChart <- getPrices ticker 
    case mChart of
        Nothing -> return (ticker, TS [] Nothing [])
        Just chart -> do 
            let seriesResult = (* (fromIntegral . snd $ quant)) 
                    <$> priceSeries' chart
            if tsNA seriesResult 
                then do 
                    error $ show quant ++ "  " ++ show (priceSeries' chart)
                    return (ticker , seriesResult)
                else return (ticker , seriesResult)

portfolioSeries :: Portfolio -> Handler (TimeSeries Double) 
portfolioSeries  (Portfolio _ ps) = do 
    tss <- mapM (fmap snd . positionSeries) ps
    if null tss
        then return $ TS [] Nothing []
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
    res <- P.readFile estimateFile  
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
            (formWidget, formEnctype) <- generateFormPost stressForm
            defaultLayout 
                    $(widgetFile "stress")


postStressR :: Handler Value
postStressR  = do
    Entity userkey _ <- requireAuth
    ((result, formWidget), formEnctype) <- runFormPost stressForm
    case result of
        FormSuccess shocks -> do
            userPositions' <- runDB $ 
                selectList 
                [PortfolioUserId ==. userkey][LimitTo 1]
            let userPositions = 
                    portfolioPortfolio . entityVal . mhead $ userPositions' 
            port <- portfolioSeries (Portfolio userkey userPositions)  
            ms <- getMacro
            debugM "Just after getMacro" 
            let df = tsRowBind  (port : ms)
                dfStr = printFrame df
                parse = map (mtail . dropWhile (/= ',')) . mtail . lines
            str <- liftIO $ do
                        tempFile <- writeTemp $ unpack dfStr
                        runRCode tempFile 
            mprint $ parse str
            mprint  str
            let [c, i, r, u] = (map P.read . parse $ str) :: [Double]
                model = Model1 c i r u
                effect = computeEffect model shocks 
            return $ toJSON effect
        FormFailure er  -> return $ toJSON er 
        _                  -> return $ toJSON ("Failed" :: String) 
    
-- Show the effect on the prtfolio 
-- I  need the betas . I need the changes
-- 1. Data tyoe for the betas. Another for the shocks
data Model1 = Model1 {
    intercept1 :: Double,
    inflation1 :: Double,
    interest1  :: Double,
    unemployment1 :: Double
} deriving (Show) 

data Shocks = Shocks {
    inflationShock :: Double,
    interestShock :: Double,
    unemploymentShock :: Double
} deriving (Show)

type Effect = Double 

computeEffect :: Model1 -> Shocks -> Effect
computeEffect (Model1 _ i r u) (Shocks is rs us) = i*is + r*rs + u*us

-- Comute effect and display.
-- What people want is reassurance
-- They need someone to tell them to watchout or not
-- The point is to give assurance.
-- You enter your portfolio and i tell you are robust or not
-- based on how diversified the portfolio is.
-- the greater the exposure to the market, the better the portfolio.
-- Three levels of robustness. Robust, Average, Fragile
-- For a robust portfolio 
-- 1. volatility is less than market vol
-- 2. Most of the vol is market driven
-- For average portfolio
-- 1. vol is about the same as market
-- 2. contains some idiosyncratic risk
-- For fragile portfolios
-- 1. the a large part of the vol is idiosyncratic or reliant on other factors
-- 2. vol is relatively large
-- So variance decomposition is key. We need a VAR model.
