{-# LANGUAGE OverloadedStrings #-}

module Data.Pad where

import           Control.Monad.Identity
import           Control.Monad.Writer

import           Data.Text              (Text)
import           Data.Text.Utils

newtype PAD = PAD (GUID, Text)
        deriving (Eq, Read, Show)
newtype PASS = PASS (GUID, Text)
        deriving (Eq, Read, Show)

type GUID = Text
type Log = Text

type ReportT m a = WriterT Log m a
type Report a = ReportT Identity a

getReportT :: Monad m => a -> (a -> ReportT m b) -> m Log
getReportT v p = execWriterT $ p v

getReport :: a -> (a -> Report b) -> Log
getReport v p = runIdentity $ getReportT v p

load :: Monad m
     => PAD
     -> ReportT m PAD
load pad = return pad

loadWith :: Monad m
         => Log
         -> PAD
         -> ReportT m PAD
loadWith message pad = tell message >> load pad

syntacticAnalysis :: Monad m
                  => PAD
                  -> ReportT m (Maybe PASS)
syntacticAnalysis (PAD (g, t)) = return $ Just $ PASS (g, "pass(" <> t <> ")")

syntacticAnalysisWith :: Monad m
                      => Log
                      -> PAD
                      -> ReportT m (Maybe PASS)
syntacticAnalysisWith message pad = tell message >> syntacticAnalysis pad

staticAnalysis :: Monad m
               => PASS
               -> ReportT m Bool
staticAnalysis pass = return True

staticAnalysisWith :: Monad m
                      => Log
                      -> PASS
                      -> ReportT m Bool
staticAnalysisWith message pass = tell message >> staticAnalysis pass

validate :: Monad m
         => PAD
         -> ReportT m Bool
validate pad = do pass_m <- syntacticAnalysis pad
                  let echec = return False
                      proceedWith = staticAnalysis
                  maybe echec proceedWith pass_m

validateWith :: Monad m
             => Log
             -> PAD
             -> ReportT m Bool
validateWith message pad = tell message >> validate pad

transmit :: Monad m
         => PAD
         -> ReportT m a
transmit = undefined


