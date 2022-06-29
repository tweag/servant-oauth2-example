module Types where

import Data.Text (Text)

data Env = Env
  { _oauthConfig :: OAuthConfig
  }

data OAuthConfig = OAuthConfig
  { _secret :: Text
  , _id     :: Text
  , _name   :: Text
  }



