{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Stress where

import Import hiding  (share)
import Utils.Functions
import Data.Aeson (eitherDecode, decode)
import Utils.Yahoo.Portfolio ( getSP, getPrices)
import Utils.Fred.Series (getMacro)
import Utils.TimeSeries
import qualified Prelude as P
import System.Directory (removeFile, getTemporaryDirectory)
import System.Process
import System.IO (hPutStr, openTempFile)
import System.FilePath.Posix (dropExtension)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C

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

runRCode :: P.FilePath -> IO (Maybe ROut)
runRCode  path = do
    let estimateFile = dropExtension path ++ ".json"
    system $ "Rscript " ++ scriptPath ++ " " ++ path 
    res <- decode <$> C.readFile estimateFile  
    removeFile path
    removeFile estimateFile
    return res

getStressR :: Handler Html
getStressR = do
    Entity userkey _ <- requireAuth
    userPositions' <- runDB $ 
        selectList 
            [PortfolioUserId ==. userkey][LimitTo 1]
    let str :: Maybe Health
        str = Nothing 
    if null userPositions' 
        then redirect PortfolioR
        else do
            (formWidget, formEnctype) <- generateFormPost stressForm
            defaultLayout 
                    $(widgetFile "stress")

postStressR :: Handler Html
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
            sp <- fmap priceSeries' <$> getSP
            let df = case sp of 
                        Just spp -> tsRowBind  (port : (ms++ [spp]))
                        Nothing  -> tsRowBind  (port : ms)
                dfStr = printFrame df
            str <- liftIO $ do
                        tempFile <- writeTemp $ unpack dfStr
                        fmap assessHealth <$> runRCode tempFile 
            defaultLayout 
                    $(widgetFile "stress")
        _                  -> redirect StressR
    
-- Show the effect on the prtfolio 
-- I  need the betas . I need the changes
-- 1. Data tyoe for the betas. Another for the shocks
data Model1 = Model1 {
    intercept1 :: (Double, Double), 
    inflation1 :: (Double, Double),
    interest1  :: (Double, Double),
    unemployment1 :: (Double, Double)
} deriving (Show) 

data Shocks = Shocks {
    inflationShock :: Double,
    interestShock :: Double,
    unemploymentShock :: Double
} deriving (Show)

type Effect = Double 

computeEffect :: Model1 -> Shocks -> Effect
computeEffect (Model1 _ (i,_) (r,_) (u,_)) (Shocks is rs us) = i*is + r*rs + u*us

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
--
-- map (Prelude.tail . (C.split ',')) . Prelude.tail . C.lines <$> C.readFile
--

assessHealth :: ROut -> Health 
assessHealth (ROut bs ss rs _) = 
    -- bs is the vector of betas, including 
    -- market beta cm
    -- rs is the R-quare relative to 
    -- the CAPM model
    let [_, _, _, _, cm] = bs
    in if rs > 0.7 && cm < 1 
            then Robust 
            else if rs < 0.4 && cm > 1
                then Fragile 
                else Average 
