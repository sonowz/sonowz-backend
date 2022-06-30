module Sonowz.Core.Web.CRUD
  ( CRUDAPI,
    CRUDHandlers (..),
    crudHandlerFromHandlers,
    crudHandlerFromDBQueries,
  )
where

import Control.Exception (AssertionFailed (AssertionFailed))
import Control.Exception.Safe qualified as E
import Data.Aeson (FromJSON, ToJSON)
import GHC.TypeLits (Symbol)
import Servant
import Sonowz.Core.DB.CRUD (CRUDQueries (..))
import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.Imports
import Sonowz.Core.StdEff.Effect (webLiftIO)

type CRUDAPI item citem uid (path :: Symbol) =
  path :> Get '[JSON] [item]
    :<|> path :> Capture "uid" uid :> Get '[JSON] item
    :<|> path :> ReqBody '[JSON] citem :> Post '[JSON] item
    :<|> path :> Capture "uid" uid :> ReqBody '[JSON] citem :> Put '[JSON] item
    :<|> path :> Capture "uid" uid :> Delete '[JSON] ()

data CRUDHandlers item citem uid r = CRUDHandlers
  { list :: Sem r [item],
    read :: uid -> Sem r item,
    create :: citem -> Sem r item,
    update :: uid -> citem -> Sem r item,
    delete :: uid -> Sem r ()
  }

crudHandlerFromHandlers ::
  forall item citem uid (path :: Symbol) r.
  (Member (Error ServerError) r, FromJSON citem, ToJSON item, FromHttpApiData uid) =>
  CRUDHandlers item citem uid r ->
  ServerT (CRUDAPI item citem uid path) (Sem r)
crudHandlerFromHandlers CRUDHandlers {..} = list :<|> read :<|> create :<|> update :<|> delete

-- Throws 500 Internal Server Error when query fails
crudHandlerFromDBQueries ::
  forall item citem uid (path :: Symbol) r.
  ( Member (Error ServerError) r,
    Members DBEffects r,
    FromJSON citem,
    ToJSON item,
    FromHttpApiData uid
  ) =>
  CRUDQueries item citem uid ->
  ServerT (CRUDAPI item citem uid path) (Sem r)
crudHandlerFromDBQueries queries = crudHandlerFromHandlers crudHandler
  where
    crudHandler =
      CRUDHandlers
        { list = withDBConn $ \conn -> webLiftIO (crudList queries conn),
          read = \uid -> withDBConn $ \conn -> webLiftIO (maybeExc =<< crudRead queries conn uid),
          create = \citem -> withDBConn $ \conn -> webLiftIO (maybeExc =<< crudCreate queries conn citem),
          update = \uid citem -> withDBConn $ \conn -> webLiftIO (maybeExc =<< crudUpdate queries conn uid citem),
          delete = \uid -> withDBConn $ \conn -> webLiftIO (boolExc =<< crudDelete queries conn uid)
        }
    maybeExc :: Maybe a -> IO a
    maybeExc (Just x) = return x
    maybeExc Nothing = E.throw (AssertionFailed "Invalid query result")
    boolExc :: Bool -> IO ()
    boolExc True = pass
    boolExc False = E.throw (AssertionFailed "Invalid query result")
