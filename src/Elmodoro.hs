{-# LANGUAGE DeriveDataTypeable #-}
module Elmodoro
  ( Elmodoro(..)
  , ElmodoroStatus(..)

  , transitionElmodoro
  ) where

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Typeable

data ElmodoroStatus = Idle | InProgress | Break | BreakPending | Completed | Aborted deriving(Eq, Show)

data Elmodoro = Elmodoro
  { workStartTime    :: POSIXTime
  , workEndTime      :: Maybe POSIXTime
  , breakStartTime   :: Maybe POSIXTime
  , breakEndTime     :: Maybe POSIXTime
  , workLength       :: NominalDiffTime
  , breakLength      :: NominalDiffTime
  , tags             :: [String]
  , status           :: ElmodoroStatus
  } deriving(Eq, Show, Typeable)

abortElmodoro :: POSIXTime -> Elmodoro -> Elmodoro
abortElmodoro curtime elmodoro =
  elmodoro { workEndTime = Just $ curtime
           , status = Aborted
           }

waitForBreak :: POSIXTime -> Elmodoro -> Elmodoro
waitForBreak curtime elmodoro =
  elmodoro { status      = BreakPending
           , workEndTime = Just curtime
           }

startBreak :: POSIXTime -> Elmodoro -> Elmodoro
startBreak curtime elmodoro =
  elmodoro { status         = Break
           , breakStartTime = Just curtime
           }

completeElmodoro :: POSIXTime -> Elmodoro -> Elmodoro
completeElmodoro curtime elmodoro =
  elmodoro { status       = Completed
           , breakEndTime = Just $ curtime
           }

transitionElmodoro :: POSIXTime -> Elmodoro -> Elmodoro
transitionElmodoro curtime elmodoro@Elmodoro { workStartTime   = start
                                             , workLength      = worklen
                                             , breakLength     = breaklen
                                             , status          = status
                                             }

  | status == Aborted                    = elmodoro
  | status == BreakPending               = startBreak curtime elmodoro
  | status == Break                      = completeElmodoro curtime elmodoro
  | workTimeLeft <= 0                    = waitForBreak curtime elmodoro
  | workTimeLeft >  0                    = abortElmodoro curtime elmodoro
  | otherwise = elmodoro where

  expectedWorkEndTime :: UTCTime
  expectedWorkEndTime = addUTCTime worklen (posixSecondsToUTCTime start)

  workTimeLeft :: NominalDiffTime
  workTimeLeft = diffUTCTime expectedWorkEndTime (posixSecondsToUTCTime curtime)
