{-# LANGUAGE RankNTypes #-}

module Graphql.Resolver.Wager where

import           Authentication.JWT
import           Authentication.Password
import           Config
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , asks
                                                )
import           Control.Monad.Trans            ( MonadIO
                                                , MonadTrans
                                                , liftIO
                                                )
import           Data.Aeson                     ( decode
                                                , encode
                                                , parseJSON
                                                , toJSON
                                                )
import           Data.Aeson.Types               ( parse
                                                , Result(..)
                                                )
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Document         ( importGQLDocument )
import           Data.Morpheus.Types
import           Data.Morpheus.Types.Internal.AST
                                                ( OperationType )
import           Data.Pool                      ( withResource )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import           Data.Time.Clock                ( getCurrentTime )
import           Database.Base
import qualified Database.Model                as DB
import           Database.PostgreSQL.Simple     ( Connection )
import           Database.User
import           Database.Wager
import           GHC.Int                        ( Int64 )
import           GHC.Float                      ( double2Float
                                                , float2Double
                                                )
import           Graphql
import           Graphql.Resolver.User

wagerTypeResolver :: GraphQL o => DB.WagerType -> Value o WagerType
wagerTypeResolver wagerType =
  let DB.WagerType { wagerTypeId, wagerTypeName } = record wagerType
  in  return WagerType { id = pure wagerTypeId, name = pure wagerTypeName }

wagerDetailsResolver :: GraphQL o => DB.WagerDetails -> Value o WagerDetails
wagerDetailsResolver wagerDetails =
  let odds = DB.wagerDetailsOdds wagerDetails
      line = DB.wagerDetailsLine wagerDetails
  in  return WagerDetails
        { odds = pure $ double2Float odds
        , line = case line of
                   Just l  -> pure (Just $ double2Float l)
                   Nothing -> pure Nothing
        }

wagerResolver :: GraphQL o => DB.Wager -> Value o Wager
wagerResolver wager =
  let
    DB.Wager { wagerId, wagerType, wagerDetails, wagerBettorId, wagerOffererId, wagerAmount, wagerDescription }
      = record wager
  in  do
        bettorUser :: DB.User <- runSelectOne (findUserByID wagerBettorId)
                                              "Invalid Bettor Id"
        offererUser :: DB.User <- runSelectOne (findUserByID wagerOffererId)
                                               "Invalid Offerer Id"
        wagerType :: DB.WagerType <- runSelectOne (getWagerTypeByID wagerType)
                                                  "Invalid wager type"

        return Wager
          { id          = pure wagerId
          , wagerType   = wagerTypeResolver wagerType
          , details     = case parse parseJSON wagerDetails of
                            Success s   -> wagerDetailsResolver s
                            Error   err -> fail err
          , bettor      = userResolver bettorUser
          , offerer     = userResolver offererUser
          , amount      = pure $ double2Float wagerAmount
          , description = pure wagerDescription
          }

myWagersResolver :: Composed QUERY [] Wager
myWagersResolver = do
  myUserId               <- requireAuthorized
  dbWagers :: [DB.Wager] <- runSelect $ findWagersByBettorID myUserId
  traverse wagerResolver dbWagers

getWagerResolver :: GetWagerArgs -> Value QUERY Wager
getWagerResolver GetWagerArgs { wagerId } = do
  _ <- requireAuthorized
  runSelectOne (findWagerByID wagerId) "Invalid wager id" >>= wagerResolver

addWagerResolver :: AddWagerArgs -> Value MUTATION Bool
addWagerResolver AddWagerArgs { wagerType, wagerDetails, bettorId, offererId, amount, description }
  = let bs :: B.ByteString                   = B.pack . T.unpack $ wagerDetails
        detailsJson :: Maybe DB.WagerDetails = decode bs
    in  do
          nInserted <- runInsert $ insertWager
            ( wagerType
            , detailsJson
            , bettorId
            , offererId
            , float2Double amount
            , description
            )
          if nInserted > 0 then return True else return False

