module Database.Wager where

import           Control.Arrow                  ( returnA )
import           Control.Monad.Trans            ( liftIO )
import           Data.Aeson
import           Data.Text                      ( Text )
import           Database.Base
import           Database.Model
import           Database.User                  ( UserID )
import           GHC.Int                        ( Int64 )
import           GHC.Generics
import           Opaleye

type WagerID = Int

wagerSelect :: Select WagerField
wagerSelect = selectTable wagerTable

wagerTypeSelect :: Select WagerTypeField
wagerTypeSelect = selectTable wagerTypeTable

insertWager
  :: ToJSON sqlJSONB => (Int, sqlJSONB, UserID, UserID, Double, Text) -> Insert Int64
insertWager (wagerType, wagerDetails, wagerBettorId, wagerOffererId, wagerAmount, wagerDescription)
  = Insert
    { iTable      = wagerTable
    , iRows       = withTimestamp
                      [ Wager { wagerId        = Nothing
                              , wagerType      = toFields wagerType
                              , wagerDetails   = toFields $ toJSON wagerDetails
                              , wagerBettorId  = toFields wagerBettorId
                              , wagerOffererId = toFields wagerOffererId
                              , wagerAmount    = toFields wagerAmount
                              , wagerDescription = toFields $ Just wagerDescription
                              }
                      ]
    , iReturning  = rCount
    , iOnConflict = Nothing
    }

findWagersByBettorID :: UserID -> Select WagerField
findWagersByBettorID id =
  proc () -> do
    wager <- wagerSelect -< ()
    let wagerDetail = record wager
    restrict -< wagerBettorId wagerDetail .== toFields id
    returnA -< wager

findWagerByID :: WagerID -> Select WagerField
findWagerByID id =
  proc () -> do
    wager <- wagerSelect -< ()
    let wagerDetail = record wager
    restrict -< wagerId wagerDetail .== toFields id
    returnA -< wager

getWagerTypeByID :: Int -> Select WagerTypeField
getWagerTypeByID id =
  proc () -> do
    wagerType <- wagerTypeSelect -< ()
    let wagerTypeDetail = record wagerType
    restrict -< wagerTypeId wagerTypeDetail .== toFields id
    returnA -< wagerType
