{-# language TypeFamilies #-}

module Auth where

import Network.Wai (Request)
import Servant (AuthProtect)
import Servant.Server.Experimental.Auth (AuthServerData, AuthHandler)

import Types

type instance AuthServerData (AuthProtect "optional-cookie-auth") = Session 'Anyone
type instance AuthServerData (AuthProtect "login") = Login
type instance AuthServerData (AuthProtect "complete") = Complete


authHandler :: AuthHandler Request (Session 'Anyone)
authHandler = undefined


loginHandler :: AuthHandler Request Login
loginHandler = undefined


completeHandler :: AuthHandler Request Complete
completeHandler = undefined
