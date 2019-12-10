{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad        ((>=>))
import           Control.Monad.Writer (tell)
import           Data.Pad             (PAD (..))
import qualified Data.Pad             as PAD
import           Data.PadActions
import           Data.Pass            (PASS)
import qualified Data.Pass            as PASS
import           Data.Text.Color
import           Data.Text.Utils
import           Test.Lib

main :: IO ()
main = run test1 >> return ()

-----------------------
-- TESTS
--

-- TEST 1

test1 = mkTest "PAD 1 load -> validate" testPAD1

testPAD1 = do -- with
              let thePAD = "pad"
                  thePASS = "pass(" <> thePAD <> ")"
                  messageLoad = "loading" <> eol
                  messageValidate = "validating" <> eol
                  expectedDiagnostic =  messageLoad
                                     <> messageValidate
              tell $ "load and validate" <> eol
              -- do
              let pad = PAD ("guid", thePAD) -- creating the PAD
                  -- processing and acquiring
                  diagnostic = getReport pad $ loadWith messageLoad >=> validateWith messageValidate
              tell $ withFG Blue "|" <> " Expected diagnostic : " <> expectedDiagnostic <> eol
              tell $ withFG Blue "|" <> "          Diagnostic : " <> diagnostic <> eol
              -- check
              return $ diagnostic == expectedDiagnostic

