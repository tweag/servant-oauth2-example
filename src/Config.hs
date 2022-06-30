module Config where

import Data.Text (Text)
import Toml      (TomlCodec, (.=), text, table , diwrap)

data OAuthConfig = OAuthConfig
  { _name   :: Text
  , _id     :: Text
  , _secret :: Text
  }

oauthConfigCodec :: TomlCodec OAuthConfig
oauthConfigCodec = OAuthConfig
  <$> diwrap (text "name")   .= _name
  <*> diwrap (text "id")     .= _id
  <*> diwrap (text "secret") .= _secret

data Config = Config
  { _oauth  :: OAuthConfig
  }

configCodec :: TomlCodec Config
configCodec = Config
  <$> table oauthConfigCodec "oauth" .= _oauth
