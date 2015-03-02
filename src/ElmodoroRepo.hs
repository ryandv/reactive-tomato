{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module ElmodoroRepo where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Lazy

import Data.Acid
import Data.IntMap.Lazy
import Data.List
import Data.SafeCopy
import Data.Time.Clock.POSIX
import Data.Typeable

import Elmodoro

data ElmodoroDB = ElmodoroDB { allElmodoros :: IntMap Elmodoro } deriving(Typeable)

$(deriveSafeCopy 0 'base ''ElmodoroStatus)
$(deriveSafeCopy 0 'base ''Elmodoro)
$(deriveSafeCopy 0 'base ''ElmodoroDB)

getAllElmodoros :: Query ElmodoroDB [Elmodoro]
getAllElmodoros = elems . allElmodoros <$> ask

getAllTags :: Query ElmodoroDB [String]
getAllTags = nub . concatMap tags . elems . allElmodoros <$> ask

createElmodoro          :: Elmodoro -> Update ElmodoroDB Key
createElmodoro elmodoro = do
  db <- get
  let elmodoros = allElmodoros db
  case maxViewWithKey elmodoros of

      Just ((maxID, _), _) -> do
        put . ElmodoroDB $ Data.IntMap.Lazy.insert (maxID + 1) elmodoro elmodoros
        return $ maxID + 1

      Nothing              -> do
        put . ElmodoroDB $ singleton 1 elmodoro
        return 1

updateElmodoro            :: Key -> POSIXTime -> Update ElmodoroDB (Maybe Elmodoro)
updateElmodoro id curtime = do
  modify $ go
  db <- get
  return $ Data.IntMap.Lazy.lookup id (allElmodoros db) where

  go (ElmodoroDB db) = ElmodoroDB $ adjust (transitionElmodoro curtime) id db

$(makeAcidic ''ElmodoroDB ['createElmodoro, 'updateElmodoro, 'getAllElmodoros, 'getAllTags])
