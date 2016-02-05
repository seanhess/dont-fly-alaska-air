{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- A good-natured, revenge-bot twitter account in thanks to @AlaskaAir for their wonderful customer service and helpful check-in policy :)
-- Just close the doors when it's too late! I can run!

import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((^.), (&), (?~))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Resource (runResourceT, ResourceT, MonadResource, MonadThrow, monadThrow)
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as S8
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Conduit hiding (responseBody, responseStatus, responseHeaders)
import Network.HTTP.Types (statusCode)
import System.Environment
import System.Random (randomRIO)
import Web.Twitter.Conduit
import Web.Twitter.Conduit.Stream (stream, statusesFilterByTrack)
import Web.Twitter.Conduit.Base (makeRequest, getResponse, sinkFromJSON)
import Web.Twitter.Conduit.Status (update)
import Web.Twitter.Types.Lens (statusText, StreamingAPI(..), Status, statusId, statusInReplyToStatusId, UserId, statusUser, userScreenName)
import Web.Authenticate.OAuth (oauthConsumerKey, oauthConsumerSecret, Credential(..))

-- the account to respond to
accountName = "AlaskaAir"
accountID = 13192972

main :: IO ()
main = do
    twToken <- getOAuthTokens
    let twInfo = TWInfo twToken Nothing

    mgr <- newManager tlsManagerSettings
    timeline <- runResourceT $ start twInfo mgr

    return ()

start :: TWInfo -> Manager -> ResourceT IO ()
start twInfo mgr = do
    -- call twInfo mgr homeTimeline
    liftIO $ putStrLn "STARTING"

    s <- retryMinuteDouble $ streamShowStatus twInfo mgr (statusesFilter accountName [accountID])

    liftIO $ putStrLn "CONNECTED"
    handleStream twInfo mgr s

    liftIO $ putStrLn "DONE"


handleStream :: (MonadResource m) => TWInfo -> Manager -> ResumableSource m StreamingAPI -> m ()
handleStream twInfo mgr s = do
    s $$+- CL.mapM_ $ \status -> (handleStatus twInfo mgr status)

handleStatus :: (MonadResource m) => TWInfo -> Manager -> StreamingAPI -> m ()
handleStatus twInfo mgr (SStatus status) =
    when (shouldRespond status) $ do
        liftIO $ putStrLn "STATUS"
        liftIO $ print status
        -- liftIO $ print (status ^. statusText)
        -- liftIO $ print (shouldRespond status)
        r <- liftIO $ randomReason
        call twInfo mgr (createResponse status r)
        return ()
handleStatus _ _ _ = return ()

createResponse :: Status -> Text -> APIRequest StatusesUpdate Status
createResponse status reason =
    update ("@" <> (status ^. statusUser . userScreenName) <> " Don't fly @AlaskaAir. " <> reason)
      & inReplyToStatusId ?~ (status ^. statusId)

dontFlyReasons :: [Text]
dontFlyReasons =
    [ "There are snakes on the plane!"
    , "They're in league with Monsanto. Say no to genetically modified aircraft!"
    , "They'll make you visit Seattle even if you don't want to."
    , "The interior of the plane is carpeted like a VW bus. #VanDownByTheRiver"
    , "You don't know where that thing has been."
    , "Who knows what's really going on in the cockpit? #CockpitTransparency"
    , "They have a strict policy of closing check-in 40 minutes early"
    , "Donald Trump likes them."
    ]

randomReason :: IO Text
randomReason = do
    n <- randomRIO (0, (length dontFlyReasons) - 1)
    return $ dontFlyReasons !! n


-- Respond unless this tweet is already itself a response to a status
shouldRespond :: Status -> Bool
shouldRespond status =
    (status ^. statusInReplyToStatusId == Nothing)

-- These must be set from the command line before running
-- See apps.twitter.com to generate some
getOAuthTokens :: IO TWToken
getOAuthTokens = do
    consumerKey <- getEnv' "TWITTER_CONSUMER_KEY"
    consumerSecret <- getEnv' "TWITTER_CONSUMER_SECRET"
    accessToken <- getEnv' "TWITTER_OAUTH_TOKEN"
    accessSecret <- getEnv' "TWITTER_OAUTH_SECRET"
    let oauth = twitterOAuth
            { oauthConsumerKey = consumerKey
            , oauthConsumerSecret = consumerSecret
            }
        cred = Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessSecret)
            ]
    return $ TWToken oauth cred
  where
    getEnv' = (S8.pack <$>) . getEnv

------------------------------------------

-- When we are rate-limited, we're supposed to back off exponentially, starting 
-- with 60s and doubling each time
retryWithDelay :: MonadIO m => Int -> (Int -> Int) -> m (Maybe a) -> m a
retryWithDelay delay grow action = do
    ma <- action
    case ma of
      Just a -> return a
      Nothing -> do
        liftIO $ putStrLn ("FAILED, delaying for " <> show delay <> "s")
        liftIO $ threadDelay (delay * 1000 * 1000)
        retryWithDelay (grow delay) grow action

retryMinuteDouble = retryWithDelay 60 (*2)

--------------------------------------

-- Forked from twitter-conduit to print out the status code
-- and handle http status errors
streamShowStatus :: (MonadResource m, FromJSON responseType)
       => TWInfo
       -> Manager
       -> APIRequest apiName responseType
       -> m (Maybe (ResumableSource m responseType))
streamShowStatus info mgr req = do
    rsrc <- getResponse info mgr =<< liftIO (makeRequest req)
    liftIO $ print (responseStatus rsrc)
    liftIO $ print (responseHeaders rsrc)
    case statusCode (responseStatus rsrc) of
      200 -> return $ Just $ responseBody rsrc $=+ CL.sequence sinkFromJSON
      _ -> return Nothing



--------------------------------------------------


-- Forked from twitter-conduit to allow multiple filters with the same stream
-- They do NOT like one program immediately connecting twice
statusesFilter :: Text -> [UserId] -> APIRequest StatusesFilter StreamingAPI
statusesFilter keyword userIds =
    APIRequestPost statusesFilterEndpoint [("track", PVString keyword),("follow", PVIntegerArray userIds)]

-- forked from twitter-conduit because it isn't exported
statusesFilterEndpoint :: String
statusesFilterEndpoint = "https://stream.twitter.com/1.1/statuses/filter.json"
