{-# language DataKinds      #-}
{-# language KindSignatures #-}
{-# language GeneralizedNewtypeDeriving      #-}

module Types where

import Control.Monad.Except   (MonadError)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader   (runReaderT, MonadReader, ReaderT)
import Servant                (Handler, ServerError)
import Web.ClientSession      (Key)

import Config


data Role = Admin | Anyone


newtype PageM' (r :: Role) a = PageM' { getPageM' :: ReaderT Env Handler a }
  deriving (Functor, Applicative, Monad, MonadError ServerError, MonadIO, MonadReader Env)


runPageM' :: Env -> PageM' r a -> Handler a
runPageM' env m = runReaderT (getPageM' m) env


type PageM      = PageM' 'Anyone
type AdminPageM = PageM' 'Admin


data Env = Env
  { _config :: Config
  , _sessionKey :: Key
  }
