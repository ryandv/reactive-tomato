module ElmodoroSpec where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

import Elmodoro

import Test.Hspec

main :: IO ()
main = hspec spec

exampleElmodoro :: Elmodoro
exampleElmodoro = Elmodoro
  { workStartTime   = 0
  , workEndTime     = Nothing
  , breakStartTime  = Nothing
  , breakEndTime    = Nothing
  , workLength      = fromInteger 1500
  , breakLength     = fromInteger 300
  , tags            = []
  , status          = InProgress
  }

spec :: Spec
spec = do
  describe "Elmodoro" $ do
    context "transitionElmodoro" $ do
      it "transitions Elmodoros to BreakPending when the work time has elapsed" $
        transitionElmodoro 1500 exampleElmodoro `shouldBe` exampleElmodoro
          { status      = BreakPending
          , workEndTime = Just 1500
          }

      it "transitions Elmodoros from BreakPending to Break" $
        let pendingBreak = exampleElmodoro { status      = BreakPending
                                           , workEndTime = Just 1500
                                           } in

          transitionElmodoro 1501 pendingBreak `shouldBe` pendingBreak
            { status = Break
            , breakStartTime = Just 1501
            }

      it "completes Elmodoros on Break before the work + break time has elapsed" $
        let elmodoroOnBreak = exampleElmodoro { status         = Break
                                              , workEndTime    = Just 1500
                                              , breakStartTime = Just 1501
                                              } in

          transitionElmodoro 1600 elmodoroOnBreak `shouldBe` elmodoroOnBreak
            { breakEndTime = Just 1600
            , status       = Completed
            }

      it "completes Elmodoros when the work + break time has elapsed" $
        let doneWorking = exampleElmodoro { status = Break
                                          , workEndTime = Just 1500
                                          , breakStartTime = Just 1501
                                          } in
        transitionElmodoro 1801 doneWorking `shouldBe` doneWorking
          { status = Completed
          , breakEndTime = Just 1801
          }

      it "aborts Elmodoros when the end time has not been reached" $
        transitionElmodoro 500 exampleElmodoro `shouldBe` exampleElmodoro
          { workEndTime = Just 500
          , status      = Aborted
          }

      it "does nothing to an already aborted Elmodoro" $
        let abortedElmodoro = exampleElmodoro { workEndTime = Just 500, status = Aborted } in
          transitionElmodoro 600 abortedElmodoro `shouldBe` abortedElmodoro
