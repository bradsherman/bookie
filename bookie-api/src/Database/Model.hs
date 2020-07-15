module Database.Model where

import           Data.Function                  ( (&) )
import           Data.Profunctor.Product.Default
                                                ( Default )
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Database.Base
import           Opaleye
import           Opaleye.Internal.Manipulation  ( Updater )

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
