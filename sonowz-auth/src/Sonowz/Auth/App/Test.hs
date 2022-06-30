module Sonowz.Auth.App.Test
  ( TestGetAPI,
    server,
  )
where

import Servant
import Sonowz.Auth.Imports
import Sonowz.Auth.Web.OAuth.Combinators
  ( LoginRedirectURL,
    RequireAuth301,
    RequireAuth401,
    RequireAuthMaybe,
    auth301,
    auth401,
    authMaybe,
  )

type TestGetAPI =
  "test"
    :> ( ("301" :> RequireAuth301 :> Get '[PlainText] Text)
           :<|> ("401" :> RequireAuth401 :> Get '[PlainText] Text)
           :<|> ("maybe" :> RequireAuthMaybe :> Get '[PlainText] Text)
       )

server :: Members '[Reader LoginRedirectURL, Error ServerError] r => ServerT TestGetAPI (Sem r)
server =
  (auth301 >=> return . show) :<|> (auth401 >=> return . show) :<|> (authMaybe >=> return . show)
