{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Analysis where

import Import
import System.Exit
import System.Process

runR :: IO ()
runR = do 
    code <- system "Rscript ~/projects/risk/analysis/forecast.R"
    case code of 
        ExitSuccess -> print "All good"
        ExitFailure c -> print $ "Something went wrong " <> show c
    
