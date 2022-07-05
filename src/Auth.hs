{-# language TypeFamilies   #-}
{-# language NamedFieldPuns #-}

module Auth where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ask)
import Data.Binary (encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as Base64
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (Status, status200)
import Network.Wai (Request)
import Network.Wai qualified as Wai
import Network.Wai.Middleware.Auth qualified as Wai
import Network.Wai.Middleware.Auth.OAuth2.Github (mkGithubProvider)
import Network.Wai.Middleware.Auth.Provider qualified as Wai
import Servant (AuthProtect, throwError, err401)
import Servant.Server.Experimental.Auth (AuthServerData, AuthHandler, mkAuthHandler)
import Web.ClientSession (Key, encryptIO)
import Web.Cookie (SetCookie(..), sameSiteStrict, defaultSetCookie)

import Config
import Types


type instance AuthServerData (AuthProtect "optional-cookie-auth") = Session 'Anyone
type instance AuthServerData (AuthProtect "login") = Login
type instance AuthServerData (AuthProtect "complete") = Complete


data OAuthAction = DoLogin | DoComplete


authHandler :: AuthHandler Request (Session 'Anyone)
authHandler = undefined


loginAuthHandler :: Env 'Anyone -> AuthHandler Request Login
loginAuthHandler env = mkAuthHandler (runPageM' env . f)
  where
    f :: Request -> PageM Login
    f request = do
      response <- runGithubAuth request (_oauth (config env)) Nothing Nothing DoLogin
      let headers = Wai.responseHeaders response
          Just location = lookup "Location" headers
      pure $ Login (decodeUtf8 location)


completeAuthHandler :: Env 'Anyone -> AuthHandler Request Complete
completeAuthHandler env = mkAuthHandler (runPageM' env . f)
  where
    f :: Request -> PageM Complete
    f request = do
      let success ident = pure $ Wai.responseLBS status200 [("Success", ident)] ""
          failure resultStatus x = pure $ Wai.responseLBS resultStatus [("Failure", x)] ""

      response <- runGithubAuth request (_oauth (config env)) (Just success) (Just failure) DoComplete

      let headers = Wai.responseHeaders response

      case lookup "Success" headers of
        Nothing -> throwError err401
        Just ident -> do
          -- We're in!
          key <- sessionKey <$> ask
          cookie <- liftIO $ buildSessionCookie key ident
          pure (Complete cookie)


ourCookie :: BS.ByteString
ourCookie = "todo_fancy_cookie_name"


buildSessionCookie :: Key -> SessionId -> IO SetCookie
buildSessionCookie key sid = do
  encrypted <- encryptIO key $ BSL.toStrict $ encode $ sid
  pure $ defaultSetCookie
    { setCookieName     = ourCookie
    , setCookieValue    = Base64.encode encrypted
    , setCookieMaxAge   = Just oneWeek
    , setCookiePath     = Just "/"
    , setCookieSameSite = Just sameSiteStrict
    , setCookieHttpOnly = True
    , setCookieSecure   = False
    }
    where
      oneWeek = 3600 * 24 * 7


runGithubAuth :: (MonadIO m)
              => Request
              -> OAuthConfig
              -> Maybe (Wai.AuthLoginState -> IO Wai.Response)
              -> Maybe (Status -> ByteString -> IO Wai.Response)
              -> OAuthAction
              -> m Wai.Response
runGithubAuth request OAuthConfig{_secret,_id,_name} msuccess mfailure action = do
  let emailAllowList = [".*"]
      appRoot  = Wai.smartAppRoot request
      provider = Wai.Provider $ mkGithubProvider _name _id _secret emailAllowList Nothing
      success  = maybe (error "'success' not implemented") id msuccess
      failure  = maybe (error "'failure' not implemented") id mfailure
      suffix = case action of
                DoLogin    -> []
                DoComplete -> ["complete"]
      providerUrl (Wai.ProviderUrl url) = Wai.mkRouteRender (Just appRoot) "auth" url provider
  liftIO $ Wai.handleLogin provider request suffix providerUrl success failure

