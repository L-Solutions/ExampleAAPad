module Main where

import           Data.Automaton         (Automaton, evaluate, runAutomaton)
import qualified Example.StartGoDoneGet as E1
import           Text.Read

main = loopE1 E1.initialAutomaton
    where loopE1 = loop E1.mkInput

loop :: (Eq i, Show o)
     => (String -> Maybe i)
     -> Automaton i o
     -> IO ()
loop mkInput aut = do putStrLn "Enter input :"
                      line <- getLine
                      maybe (return ()) runEvaluation $ mkInput line
    where runEvaluation input = do let (o, newAut) = runAutomaton (evaluate input) aut
                                   print o
                                   loop mkInput newAut

--
