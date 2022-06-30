{-# language DataKinds      #-}
{-# language KindSignatures #-}
{-# language GeneralizedNewtypeDeriving      #-}

module Types where

import Control.Monad.Except (MonadError)
import Servant              (Handler, ServerError)

import Config


data Role = Admin | Anyone


newtype PageM' (r :: Role) a = PageM' { getPageM' :: Handler a }
  deriving (Functor, Applicative, Monad, MonadError ServerError)


type PageM      = PageM' 'Anyone
type AdminPageM = PageM' 'Admin


data Env = Env
  { _config :: Config
  }
