{-# language DataKinds      #-}
{-# language KindSignatures #-}

module Types where

import Control.Monad.Reader  (ReaderT)
import Data.Text             (Text)
import Servant               (Handler)


data Role = Anyone | Admin


data Env r = Env
  { session :: Maybe (Session r)
  }


initialEnv :: Env r
initialEnv = Env
  { session = Nothing
  }

data User = User
  { email :: Text
  }


data Session (r :: Role) = Session
  { user :: Maybe User
  }


type PageM      = ReaderT (Env 'Anyone) Handler
type AdminPageM = ReaderT (Env 'Admin)  Handler
