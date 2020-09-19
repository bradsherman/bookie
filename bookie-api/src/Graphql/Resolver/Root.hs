module Graphql.Resolver.Root where

import           Data.Morpheus.Types
import           Graphql
import           Graphql.Resolver.User
import           Graphql.Resolver.Wager

rootResolver :: RootResolver Web () Query Mutation Undefined
rootResolver = RootResolver { queryResolver
                            , mutationResolver
                            , subscriptionResolver
                            }
 where
  queryResolver = Query { login      = loginResolver
                        , myUserInfo = myUserInfoResolver
                        , hello      = helloResolver
                        , myWagers   = myWagersResolver
                        , getWager   = getWagerResolver
                        }
  mutationResolver = Mutation { register       = registerResolver
                              , changePassword = changePasswordResolver
                              , addWager       = addWagerResolver
                              }
  subscriptionResolver = Undefined
