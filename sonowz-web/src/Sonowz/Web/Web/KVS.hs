module Sonowz.Web.Web.KVS
  ( KVSAPI,
    kvsAPIHandler,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Servant
import Sonowz.Auth.OAuth.Types (UserInfo (oauthId))
import Sonowz.Auth.Web.OAuth.Combinators (RequireAuth401, auth401)
import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Web.Imports
import Sonowz.Web.KVS.DB.Queries (deleteKey, getKeyValue, setKeyValue)

type KVSAPI =
  RequireAuth401
    :> Capture "key" Text
    :> ( Get '[JSON] ValueBody
           :<|> (ReqBody '[JSON] ValueBody :> Post '[JSON] Response)
           :<|> Delete '[JSON] Response
       )

newtype ValueBody = ValueBody {value :: Text} deriving (Generic)

instance ToJSON ValueBody

instance FromJSON ValueBody

newtype Response = Response {message :: Text} deriving (Generic)

instance ToJSON Response

type KVSAPIEffects = Error ServerError : DBEffects

kvsAPIHandler :: forall r. Members KVSAPIEffects r => ServerT KVSAPI (Sem r)
kvsAPIHandler auth key = handlerGet :<|> handlerPost :<|> handlerDelete
  where
    getUserId = oauthId <$> auth401 auth :: Sem r Text
    handlerGet = do
      userId <- getUserId
      maybeValue <- withDBConn $ \conn -> webLiftIO (getKeyValue conn userId key)
      case maybeValue of
        Just value -> return (ValueBody value)
        Nothing -> throw err400
    handlerPost (value -> valueText) = do
      userId <- getUserId
      withDBConn $ \conn -> webLiftIO (setKeyValue conn userId key valueText)
      return $ Response $ "'" <> key <> "' successfully set."
    handlerDelete = do
      userId <- getUserId
      withDBConn $ \conn -> webLiftIO (deleteKey conn userId key)
      return $ Response $ "'" <> key <> "' key successfully deleted."
