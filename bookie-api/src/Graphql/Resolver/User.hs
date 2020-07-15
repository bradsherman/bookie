module Graphql.Resolver.User where

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
import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Document         ( importGQLDocument )
import           Data.Morpheus.Types
import           Data.Morpheus.Types.Internal.AST
                                                ( OperationType )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import           Data.Time.Clock                ( getCurrentTime )
import           Database.Base
import qualified Database.Model                as DB
import           Database.PostgreSQL.Simple     ( Connection )
import           Database.User
import           GHC.Int                        ( Int64 )
import           GHC.Float                      ( double2Float
                                                , float2Double
                                                )
import           Graphql

-------------------------------------------------------------------------------
userResolver :: GraphQL o => DB.User -> Object o User
userResolver user =
  let DB.User { userId, userEmail, userFirstName, userLastName, userUnitSize }
        = record user
      floatUnitSize = double2Float userUnitSize
  in  return User { id        = pure userId
                  , email     = pure userEmail
                  , firstName = pure userFirstName
                  , lastName  = pure userLastName
                  , unitSize  = pure floatUnitSize
                  , createdAt = pure . T.pack . show $ recordCreatedAt user
                  , updatedAt = pure . T.pack . show $ recordUpdatedAt user
                  }

-------------------------------------------------------------------------------
loginResolver :: GraphQL o => LoginArgs -> Object o Session
loginResolver LoginArgs { email, password } = do
  res :: [DB.User] <- runSelect $ findUserByEmail email
  case res of
    [user]
      | validateHashedPassword (DB.userPasswordHash . record $ user) password -> do
        time   <- liftIO getCurrentTime
        secret <- lift $ asks (jwtSecret . config)
        let jwt = makeJWT time secret (DB.userId . record $ user)
        return Session { token = pure jwt, user = userResolver user }
    _ -> failRes "Wrong email or password"

-------------------------------------------------------------------------------
registerResolver :: RegisterArgs -> Object MUTATION Session
registerResolver RegisterArgs { email, password, firstName, lastName, unitSize }
  = do
    res :: [DB.User] <- runSelect $ findUserByEmail email
    case res of
      _ : _ -> failRes "This email is already taken"
      []    -> do
        ph <- liftIO $ hashPassword password
        runInsert
          $ insertUser (email, ph, firstName, lastName, float2Double unitSize)
        loginResolver LoginArgs { email, password }

-------------------------------------------------------------------------------
myUserInfoResolver :: Object QUERY User
myUserInfoResolver = do
  myUserId <- requireAuthorized
  runSelectOne (findUserByID myUserId) "Invalid user" >>= userResolver


helloResolver :: Value QUERY Text
helloResolver = return "Hello World!"

-------------------------------------------------------------------------------
changePasswordResolver :: ChangePasswordArgs -> Value MUTATION Bool
changePasswordResolver ChangePasswordArgs { oldPassword, newPassword } = do
  myUserId            <- requireAuthorized
  userData :: DB.User <- runSelectOne (findUserByID myUserId) "Invalid user"
  if validateHashedPassword (DB.userPasswordHash . record $ userData)
                            oldPassword
    then do
      ph <- liftIO $ hashPassword newPassword
      runUpdate $ updateUserPassword myUserId ph
      return True
    else failRes "Wrong old password"
