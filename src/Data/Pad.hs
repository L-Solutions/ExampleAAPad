{-# LANGUAGE OverloadedStrings #-}

module Data.Pad where

import           Control.Monad.Writer

import           Data.Text            (Text)
import           Data.Text.Utils

newtype PAD = PAD Text
        deriving (Eq, Read, Show)
newtype PASS = PASS Text
        deriving (Eq, Read, Show)
type PADID = Int

type Log = Text

type DiagnosticT m a = WriterT Log m a
type Diagnostic a = Writer Log a

getDiagnostic :: a -> (a -> Diagnostic b) -> Log
getDiagnostic v p = execWriter $ p v

type ReportT m a = WriterT Log m a
type Report a = Writer Log a

load :: Log -> PAD -> Diagnostic PAD
load message p@(PAD pad) = do tell $ "load PAD : " <> pad <> eol
                              tell $ message
                              return p

syntacticAnalysis :: Log -> PAD -> Diagnostic (Maybe PASS)
syntacticAnalysis message (PAD pad) = do tell $ "read PAD : " <> pad <> eol
                                         tell $ message
                                         return $ Just $ PASS $ "pass(" <> pad <> ")"

staticAnalysis :: Log -> PASS -> Diagnostic Bool
staticAnalysis message (PASS pass) = do tell $ "read PASS : " <> pass <> eol
                                        tell $ message
                                        return True

validate :: Log -> PAD -> Diagnostic Bool
validate message pad = do tell message
                          pass_m <- syntacticAnalysis "" pad
                          let echec = return False
                              proceedWith = staticAnalysis ""
                          maybe echec proceedWith pass_m

transmit :: PAD -> Report a
transmit = undefined


