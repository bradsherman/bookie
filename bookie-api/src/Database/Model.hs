{-# LANGUAGE DeriveGeneric #-}
module Database.Model where

import           Data.Aeson
import           Data.Function                  ( (&) )
import           Data.Profunctor.Product.Default
                                                ( Default )
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Database.Base
import           GHC.Generics
import           Opaleye
import           Opaleye.Internal.Manipulation  ( Updater )
import           Opaleye.Internal.RunQuery      ( QueryRunnerColumnDefault )

-- TODO move entities into separate folders with Model and Operations
-------------------------------------------------------------------------------
data UserT a b c d e f
  = User
      { userId :: a,
        userEmail :: b,
        userPasswordHash :: c,
        userFirstName :: d,
        userLastName :: e,
        userUnitSize :: f
      }

type User = Entity (UserT Int Text Text Text Text Double)

type UserWriteField
  = EntityWriteField
      ( UserT
          (Maybe (F SqlInt4)) -- use Maybe because we don't need to specify id when inserting
          (F SqlText)
          (F SqlText)
          (F SqlText)
          (F SqlText)
          (F SqlFloat8)
      )

type UserField
  = EntityField
      ( UserT
          (F SqlInt4)
          (F SqlText)
          (F SqlText)
          (F SqlText)
          (F SqlText)
          (F SqlFloat8)
      )

-- https://hackage.haskell.org/package/product-profunctors-0.10.0.1/docs/Data-Profunctor-Product-TH.html
$(makeAdaptorAndInstance "pUser" ''UserT)

userTable :: Table UserWriteField UserField
userTable = table "users" . pEntity . withTimestampFields $ pUser User
  { userId           = tableField "id"
  , userEmail        = tableField "email"
  , userPasswordHash = tableField "password_hash"
  , userFirstName    = tableField "first_name"
  , userLastName     = tableField "last_name"
  , userUnitSize     = tableField "unit_size"
  }
-------------------------------------------------------------------------------
data WagerTypeT a b
  = WagerType
      {
        wagerTypeId :: a,
        wagerTypeName :: b
      }
  deriving (Show)

type WagerType = Entity (WagerTypeT Int Text)

type WagerTypeWriteField
  = EntityWriteField (WagerTypeT (Maybe (F SqlInt4)) (F SqlText))

type WagerTypeField = EntityField (WagerTypeT (F SqlInt4) (F SqlText))

$(makeAdaptorAndInstance "pWagerType" ''WagerTypeT)

wagerTypeTable :: Table WagerTypeWriteField WagerTypeField
wagerTypeTable =
  table "wager_type" . pEntity . withTimestampFields $ pWagerType WagerType
    { wagerTypeId   = tableField "id"
    , wagerTypeName = tableField "wager_type"
    }
-------------------------------------------------------------------------------
data WagerDetails
  = WagerDetails
      { wagerDetailsOdds :: Double,
        wagerDetailsLine :: Maybe Double
      }
  deriving (Generic,Show)

instance ToJSON WagerDetails where
  toJSON WagerDetails { wagerDetailsOdds, wagerDetailsLine } =
    object ["odds" .= wagerDetailsOdds, "line" .= wagerDetailsLine]
instance FromJSON WagerDetails where
  parseJSON = withObject "WagerDetails" $ \o -> do
    wagerDetailsOdds <- o .: "odds"
    wagerDetailsLine <- o .:? "line"
    return WagerDetails { wagerDetailsOdds, wagerDetailsLine }
-- TODO define this so I can just have opaleye translate to WagerDetails
-- instead of an Aeson Value
--
-- need to define this to run queries on wager details
-- instance QueryRunnerColumnDefault PGJsonb WagerDetails where
--   queryRunnerColumnDefault :: _ = fieldQueryRunnerColumn jsonFieldParser
  -- queryRunnerColumn
  --   (unsafeCoerceColumn :: Column WagerDetails -> Column SqlJsonb)
  --   WagerDetails
  --   queryRunnerColumnDefault

data WagerT a b c d e f g
  = Wager
      { wagerId :: a,
        wagerType :: b,
        wagerDetails :: c,
        wagerBettorId :: d,
        wagerOffererId :: e,
        wagerAmount :: f,
        wagerDescription :: g
      }

type Wager = Entity (WagerT Int Int Data.Aeson.Value Int Int Double Text)

type WagerWriteField
  = EntityWriteField
      ( WagerT
          (Maybe (F SqlInt4))
          (F SqlInt4)
          (F SqlJsonb)
          (F SqlInt4)
          (F SqlInt4)
          (F SqlFloat8)
          (Maybe (F SqlText))
      )

type WagerField
  = EntityField
      ( WagerT
          (F SqlInt4)
          (F SqlInt4)
          (F SqlJsonb)
          (F SqlInt4)
          (F SqlInt4)
          (F SqlFloat8)
          (F SqlText)
      )

$(makeAdaptorAndInstance "pWager" ''WagerT)

wagerTable :: Table WagerWriteField WagerField
wagerTable = table "wagers" . pEntity . withTimestampFields $ pWager Wager
  { wagerId          = tableField "id"
  , wagerType        = tableField "wager_type_id"
  , wagerDetails     = tableField "wager_details"
  , wagerBettorId    = tableField "bettor_id"
  , wagerOffererId   = tableField "offerer_id"
  , wagerAmount      = tableField "amount"
  , wagerDescription = tableField "description"
  }

