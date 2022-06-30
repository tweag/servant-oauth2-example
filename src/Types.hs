{-# language DataKinds      #-}
{-# language KindSignatures #-}

module Types where

import Data.Text (Text)
import Servant   (Handler)

data Role = Admin | Anyone

newtype PageM' (r :: Role) a = PageM' { getPageM' :: Handler a }

type PageM      = PageM' 'Anyone
type AdminPageM = PageM' 'Admin


data Env = Env
  { _oauthConfig :: OAuthConfig
  }


data OAuthConfig = OAuthConfig
  { _secret :: Text
  , _id     :: Text
  , _name   :: Text
  }
