{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Graphql where

import Authentication.JWT
import Authentication.Password
import Config
import Control.Monad.Base (MonadBase)
import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.Trans (MonadIO, MonadTrans, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types
import Data.Morpheus.Types.Internal.AST (OperationType)
import Data.Pool (Pool, withResource)
import Data.Profunctor.Product.Default (Default)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Clock (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import GHC.Int (Int64)
import Network.HTTP.Types (Status)
import qualified Opaleye
import Opaleye (FromFields, Insert, Select, Update)

--
-------------------------------------------------------------------------------

data Env
  = Env
      { dbPool :: Pool Connection,
        config :: Config,
        currentUserId :: Maybe Int
      }

data Error
  = Error
      Int -- HTTP status code
      B.ByteString -- Error body
  deriving (Show, Ord, Eq)

newtype Web a
  = Web
      { runWeb :: ExceptT Error (ReaderT Env IO) a
      }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadError Error,
      MonadIO,
      MonadBase IO,
      MonadBaseControl IO
    )

-- |
-------------------------------------------------------------------------------
importGQLDocument "schema.graphql"

-------------------------------------------------------------------------------

-- | Resolve single value
type Value (o :: OperationType) a = Resolver o () Web a

-- | Resolve object (which includes other fields that need their own resolvers)
type Object (o :: OperationType) a = Resolver o () Web (a (Resolver o () Web))

-- | Resolve (Maybe object)
type MaybeObject (o :: OperationType) a =
  Resolver o () Web (Maybe (a (Resolver o () Web)))

-- | Resolve [object]
type ArrayObject (o :: OperationType) a =
  Resolver o () Web [a (Resolver o () Web)]

type GraphQL o =
  ( MonadIO (Resolver o () Web),
    WithOperation o,
    MonadTrans (Resolver o ())
  )

-------------------------------------------------------------------------------

-- |
runSelect ::
  GraphQL o =>
  Default FromFields fields haskells =>
  Select fields ->
  Value o [haskells]
runSelect select = do
  db <- lift $ asks dbPool
  liftIO
    $ withResource db
    $ \connection -> Opaleye.runSelect connection select

-------------------------------------------------------------------------------
runSelectOne ::
  GraphQL o =>
  Default FromFields fields haskells =>
  Select fields ->
  String ->
  Value o haskells
runSelectOne select errorMsg = do
  db <- lift $ asks dbPool
  xs <-
    liftIO
      $ withResource db
      $ \connection -> Opaleye.runSelect connection select
  case xs of
    [x] -> return x
    _ -> failRes errorMsg

-------------------------------------------------------------------------------

-- |
runInsert :: GraphQL o => Insert haskells -> Value o haskells
runInsert insert = do
  db <- lift $ asks dbPool
  liftIO
    $ withResource db
    $ \connection -> Opaleye.runInsert_ connection insert

-------------------------------------------------------------------------------
runUpdate :: GraphQL o => Update haskells -> Value o haskells
runUpdate update = do
  db <- lift $ asks dbPool
  liftIO
    $ withResource db
    $ \connection -> Opaleye.runUpdate_ connection update

-------------------------------------------------------------------------------
requireAuthorized :: GraphQL o => Value o Int
requireAuthorized = do
  maybeID <- lift $ asks currentUserId
  case maybeID of
    Just id -> return id
    _ -> lift $ throwError (Error 401 "Unauthorized")
