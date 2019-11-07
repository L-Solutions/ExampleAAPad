{-# LANGUAGE OverloadedStrings #-}

module Example.StartGoDoneGet (
      InputType (..)
    , InputArgs (..)
    , Input (..)
    , Output (..)
    , Result (..)
    , initialAutomaton
    , mkInput
) where

import           Control.Monad
import           Control.Monad.Writer (Writer (..), runWriter, tell)
import           Data.Automaton       (Outcome (..))
import qualified Data.Automaton       as A
import           Data.Pad
import           Data.Text
import           Data.Text.Utils
import           Text.Read

-- type MessageType = Load | Validate | Send | Report

data InputType = Load | Validate | Report
  deriving (Show, Read, Eq)

type InputArgs = Either PAD GUID

data Input = Input InputType InputArgs
  deriving (Show, Read, Eq)

mkInput :: String -> Maybe Input
mkInput t = readMaybe $ "Input " ++ t

data Output = Ok GUID
            | Content Log
            | Error
            | Ko
  deriving (Show, Read, Eq)

type Result = Outcome Output

-- ============================================
-- Automaton State
type AutomatonState = A.AutomatonState Input (Report Output)

states :: [ AutomatonState ]
states = [startState, goState, doneState]

-- Start

startState = A.AutomatonState "Start" actionStart
actionStart _ = let message = "In Start State" in tell message >> return Ko

-- Go

goState = A.AutomatonState "Go" actionGo

actionGo :: Input -> Report Output
actionGo (Input Load (Left (PAD (g,t)))) = tell ("Load " <> t <> eol) >> return (Ok g)
actionGo _                               = tell ("Wrong input" <> eol) >> return Error

message t =  "The PAD " <> t <> " dd"

-- Done

doneState = A.AutomatonState "Done" actionDone

actionDone :: Input -> Report Output
actionDone (Input Validate (Right g)) = tell ("Validation for " <> g <> eol) >> return (Ok g)
actionDone _                          = tell ("Wrong input" <> eol) >> return Error

-- get

getState = A.AutomatonState "Get" actionGet

actionGet :: Input -> Report Output
actionGet (Input Report (Right g)) = tell ("Sending for " <> g <> eol) >> return (Ok g)
actionGet _                        = tell ("Wrong input" <> eol) >> return Error

-- ============================================
-- Automaton Transition
type AutomatonTransition = A.AutomatonTransition Input (Report Output)

transitions :: [ AutomatonTransition ]
transitions = [t1, t2, t3, t4, t5, t6]

t1 = A.AutomatonTransition "load PAD" startState goState guard
    where guard (Input Load _) = True
          guard _              = False

t2 = A.AutomatonTransition "validate PAD" goState doneState guard
    where guard (Input Validate _) = True
          guard _                  = False

t3 = A.AutomatonTransition "send report" doneState getState guard
    where guard (Input Report _) = True
          guard _                = False

t4 = A.AutomatonTransition "validate PAD" doneState doneState guard
    where guard (Input Validate _) = True
          guard _                  = False

t5 = A.AutomatonTransition "send report" getState getState guard
    where guard (Input Report _) = True
          guard _                = False

t6 = A.AutomatonTransition "validate PAD" getState doneState guard
    where guard (Input Validate _) = True
          guard _                  = False


-- ============================================
-- Automaton
type Automaton = A.Automaton Input (Report Output)

initialAutomaton = A.Automaton startState transitions

-- ============================================
-- Automaton Execution
type AutomatonExecution o = A.AutomatonExecution Input o (Outcome o)

