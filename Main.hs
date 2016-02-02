{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON, fromJSON, Result(..), json, Value)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import qualified Data.Conduit.Attoparsec as CA
import Control.Monad.Trans.Resource (runResourceT, ResourceT, MonadResource, MonadThrow, monadThrow)
import Control.Concurrent (forkIO, threadDelay)
import Network.HTTP.Types (statusCode)
import Web.Twitter.Conduit
import Web.Twitter.Conduit.Stream (stream, statusesFilterByTrack)
import Web.Twitter.Conduit.Base (makeRequest, getResponse, sinkFromJSON)
import Web.Twitter.Types.Lens
import Web.Authenticate.OAuth
import Network.HTTP.Conduit hiding (responseBody, responseStatus, responseHeaders)
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Control.Monad.IO.Class
import Control.Lens
import System.Environment

import qualified Data.ByteString.Char8 as S8

-- when someone mentions AlaskaAir? Wait, that could get excessive. I don't want to reply to each one!
-- when AlaskaAir writes a tweet
-- when someone tweets at AlaskaAir without it being a reply

-- shouldreply: if the status text

accountName = "dontflyalaska"
accountID = 4849803320

main :: IO ()
main = do
    twToken <- getOAuthTokens
    let twInfo = TWInfo twToken Nothing -- (Just (Proxy "127.0.0.1" 8888))

    mgr <- newManager tlsManagerSettings
    timeline <- runResourceT $ do
      -- call twInfo mgr homeTimeline
      liftIO $ putStrLn "STARTING"

      s <- retryWithDelay 60 (*2) $ streamShowStatus twInfo mgr (statusesFilter accountName [accountID])

      liftIO $ putStrLn "CONNECTED"
      handleStream s

      liftIO $ putStrLn "DONE"

    return ()
    -- timeline <- withManager $ \mgr -> 

-- connectStream :: (MonadResource m) => ResumableSource m responseType -> ResourceT IO ()
-- connectStream ss = ss $$+- CL.mapM_ $ \status -> liftIO (handleStream status)

retryWithDelay :: MonadIO m => Int -> (Int -> Int) -> m (Maybe a) -> m a
retryWithDelay delay grow action = do
    ma <- action
    case ma of
      Just a -> return a
      Nothing -> do
        liftIO $ putStrLn ("FAILED, delaying for " <> show delay <> "s")
        liftIO $ threadDelay (delay * 1000 * 1000)
        retryWithDelay (grow delay) grow action

statusesFilterEndpoint :: String
statusesFilterEndpoint = "https://stream.twitter.com/1.1/statuses/filter.json"

statusesFilter :: Text -> [UserId] -> APIRequest StatusesFilter StreamingAPI
statusesFilter keyword userIds =
    APIRequestPost statusesFilterEndpoint [("track", PVString keyword),("follow", PVIntegerArray userIds)]

handleStream :: ResumableSource (ResourceT IO) StreamingAPI -> ResourceT IO ()
handleStream s = do
    s $$+- CL.mapM_ $ \status -> liftIO (handleStatus status)

handleStatus :: StreamingAPI -> IO ()
handleStatus (SStatus status) = do
    putStrLn "STATUS"
    print (status ^. statusText)
    print (shouldRespond status)
    -- print status
handleStatus _ = return ()
-- handleStream (SRetweetedStatus _) = putStrLn "RETWEET"
-- handleStream (SEvent e) = print e
-- handleStream (SDelete _) = putStrLn "DELETE"
-- handleStream (SFriends f) = print f
-- handleStream (SUnknown v) = print v

-- respond if it's not a reply at all?
-- statusInReplyToStatusId
-- statusInReplyToUserId
-- statusInReplyToScreenName
shouldRespond :: Status -> Bool
shouldRespond status =
    -- (status ^. statusInReplyToUserId == Nothing) &&
    (status ^. statusInReplyToStatusId == Nothing)
    -- (status ^. statusInReplyToScreenName == Nothing)


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


--------------------------------------

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
