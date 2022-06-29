module Types where

import Data.Text (Text)
import Servant   (Handler)

type PageM = Handler


data Env = Env
  { _oauthConfig :: OAuthConfig
  }


data OAuthConfig = OAuthConfig
  { _secret :: Text
  , _id     :: Text
  , _name   :: Text
  }
