{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Control.Monad.Trans.Resource (runResourceT, ResourceT, MonadResource)
import Control.Concurrent (forkIO)
import Web.Twitter.Conduit
import Web.Twitter.Conduit.Stream (stream, statusesFilterByTrack)
import Web.Twitter.Types.Lens
import Web.Authenticate.OAuth
import Network.HTTP.Conduit
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
    (oauth, cred) <- getOAuthTokens
    let twInfo = setCredential oauth cred def

    mgr <- newManager tlsManagerSettings
    timeline <- runResourceT $ do
      -- call twInfo mgr homeTimeline
      liftIO $ putStrLn "STARTING"

      -- ss1 <- stream twInfo mgr (statusesFilterByTrack accountName)
      ss2 <- stream twInfo mgr (statusesFilterByFollow [4849803320])

      liftIO $ putStrLn "RUNNING"

      -- mapM connectToStream [ss1, ss2]
      -- mapM conn
      -- connectToStream ss2
      ss2 $$+- CL.mapM_ $ \status -> liftIO (handleStream status)

      liftIO $ putStrLn "DONE"

    return ()
    -- timeline <- withManager $ \mgr -> 

-- connectStream :: (MonadResource m) => ResumableSource m responseType -> ResourceT IO ()
-- connectStream ss = ss $$+- CL.mapM_ $ \status -> liftIO (handleStream status)

connectToStream :: ResumableSource (ResourceT IO) StreamingAPI -> ResourceT IO ()
connectToStream s = do
    liftIO $ forkIO $ runResourceT $ do
      s $$+- CL.mapM_ $ \status -> liftIO (handleStream status)
    return ()

handleStream :: StreamingAPI -> IO ()
handleStream (SStatus status) = do
    putStrLn "STATUS"
    print (status ^. statusText)
    print (shouldRespond status)
    -- print status
handleStream _ = return ()
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

    -- not . Text.isPrefixOf ("@" <> accountName) . Text.toLower . (^. statusText)

getOAuthTokens :: IO (OAuth, Credential)
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
    return (oauth, cred)
  where
    getEnv' = (S8.pack <$>) . getEnv
