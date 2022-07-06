{-# language DataKinds                  #-}
{-# language KindSignatures             #-}
{-# language GeneralisedNewtypeDeriving #-}

module Types where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Servant (Handler, ServerError)
import Web.ClientSession (Key)
import Web.Cookie (SetCookie)

import Config


data Role = Anyone | Admin


data Env r = Env
  { session    :: Maybe (Session r)
  , config     :: Config
  , sessionKey :: Key
  }


initialEnv :: Config -> Key -> Env r
initialEnv c k = Env
  { session    = Nothing
  , config     = c
  , sessionKey = k
  }

data User = User
  { email :: Text
  }
  deriving stock Show


data Login = Login
      { location :: Text
      }

data Complete = Complete
      { cookie :: SetCookie
      }


data Session (r :: Role) = Session
  { user :: Maybe User
  }

newtype PageM' (r :: Role) a = PageM' { getPageM' :: ReaderT (Env r) Handler a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError ServerError
           , MonadIO
           , MonadReader (Env r)
           )

runPageM' :: Env r -> PageM' r a -> Handler a
runPageM' env m = runReaderT (getPageM' m) env


type PageM      = PageM' 'Anyone
type AdminPageM = PageM' 'Admin


type SessionId = ByteString
