module Graphql.Resolver.Root where

import           Data.Morpheus.Types
import           Graphql
import           Graphql.Resolver.User

rootResolver :: RootResolver Web () Query Mutation Undefined
rootResolver = RootResolver { queryResolver
                            , mutationResolver
                            , subscriptionResolver
                            }
 where
  queryResolver = Query { login      = loginResolver
                        , myUserInfo = myUserInfoResolver
                        , hello      = helloResolver
                        }
  mutationResolver = Mutation { register       = registerResolver
                              , changePassword = changePasswordResolver
                              }
  subscriptionResolver = Undefined
