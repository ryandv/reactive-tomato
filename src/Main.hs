{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.Acid
import Data.Acid.Advanced
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.IntMap.Lazy
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Elmodoro
import ElmodoroRepo

import Happstack.Server

import System.Environment

data IdentifiedElmodoro = IdentifiedElmodoro
  { elmodoroID    :: Int
  , elmodoroModel :: Elmodoro
  }

instance ToJSON IdentifiedElmodoro where
  toJSON IdentifiedElmodoro
    { elmodoroID  = id
    , elmodoroModel = Elmodoro
      { workStartTime   = workstart
      , workEndTime     = workend
      , breakStartTime   = breakstart
      , breakEndTime     = breakend
      , workLength  = worklen
      , breakLength = breaklen
      , tags        = tags
      , status      = status
      }
    } = object [ "id"          .= id
               , "workstarttime"   .= (floor . (*1000) $ workstart :: Int)
               , "workendtime"     .= (floor . (*1000) <$> workend :: Maybe Int)
               , "breakstarttime"   .= (floor . (*1000) <$> breakstart :: Maybe Int)
               , "breakendtime"     .= (floor . (*1000) <$> breakend :: Maybe Int)
               , "worklength"  .= ((floor worklen :: Int) * 1000)
               , "breaklength" .= ((floor breaklen :: Int) * 1000)
               , "tags"        .= tags
               , "status"      .= (Prelude.map toLower . show $ status)
               ]

data ElmodoroRequest = ElmodoroRequest
  { reqWorkLength  :: Double
  , reqBreakLength :: Double
  , reqTags        :: [String]
  }

elmodoroRequestInSeconds :: Double -> Double -> [String] -> ElmodoroRequest
elmodoroRequestInSeconds worklen breaklen tags = ElmodoroRequest (worklen / 1000)
                                                                 (breaklen / 1000)
                                                                 (tags)

instance FromJSON ElmodoroRequest where
  parseJSON (Object o) =
    elmodoroRequestInSeconds <$>
      o .: "worklength"  <*>
      o .: "breaklength" <*>
      o .: "tags"

createHandler    :: AcidState ElmodoroDB -> ServerPart Response
createHandler db = do
  req <- askRq
  rqbody <- takeRequestBody req

  if (isJust rqbody)
    then do

      let reqelmodoro = decode (unBody . fromJust $ rqbody)

      case reqelmodoro of
        Just (elmodoro) -> do

          curtime <- liftIO $ getPOSIXTime

          let newelmodoro = Elmodoro { workStartTime = curtime
            , workEndTime        = Nothing
            , breakStartTime     = Nothing
            , breakEndTime       = Nothing
            , workLength         = fromRational . toRational . reqWorkLength $ elmodoro
            , breakLength        = fromRational . toRational . reqBreakLength $ elmodoro
            , tags               = reqTags (elmodoro)
            , status             = InProgress
            }

          newid <- update' db (CreateElmodoro newelmodoro)

          ok $ toResponseBS (C.pack "application/json") (encode $ IdentifiedElmodoro newid newelmodoro)

        _ -> badRequest $ toResponse ("badRequest" :: String)

    else internalServerError $ toResponse ("500" :: String)

updateHandler       :: AcidState ElmodoroDB -> Int -> ServerPart Response
updateHandler db id = do
  method PUT
  curtime <- liftIO $ getPOSIXTime

  updatedElmodoro <- update' db (UpdateElmodoro id curtime)

  case updatedElmodoro of
    (Just elmodoro) -> ok $ toResponseBS (C.pack "application/json") (encode $ IdentifiedElmodoro id elmodoro)
    Nothing         -> notFound $ toResponse ("Elmodoro not found" :: String)

main :: IO ()
main = do
  db <- openLocalState (ElmodoroDB Data.IntMap.Lazy.empty)

  envPort <- getEnv "PORT"
  simpleHTTP (nullConf { port = read envPort }) $
    msum [ dir "elmodoro" $
      msum [ do nullDir
                method POST
                createHandler db
           , path $ updateHandler db
           ]
         , dir "js" $ serveDirectory DisableBrowsing ["index.html"] "static/js"
         , dir "css" $ serveDirectory DisableBrowsing ["index.html"] "static/css"
         , dir "elm" $ serveDirectory DisableBrowsing ["index.html"] "elm-stuff/packages/elm-lang/core/1.1.0/src/Native"
         , do nullDir
              serveFile (guessContentTypeM mimeTypes) "index.html"
         ]
