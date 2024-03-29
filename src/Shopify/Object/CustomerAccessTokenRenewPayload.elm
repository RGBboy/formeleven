-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.CustomerAccessTokenRenewPayload exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Shopify.InputObject
import Shopify.Interface
import Shopify.Object
import Shopify.Scalar
import Shopify.ScalarCodecs
import Shopify.Union


{-| The renewed customer access token object.
-}
customerAccessToken :
    SelectionSet decodesTo Shopify.Object.CustomerAccessToken
    -> SelectionSet (Maybe decodesTo) Shopify.Object.CustomerAccessTokenRenewPayload
customerAccessToken object____ =
    Object.selectionForCompositeField "customerAccessToken" [] object____ (Basics.identity >> Decode.nullable)


{-| The list of errors that occurred from executing the mutation.
-}
userErrors :
    SelectionSet decodesTo Shopify.Object.UserError
    -> SelectionSet (List decodesTo) Shopify.Object.CustomerAccessTokenRenewPayload
userErrors object____ =
    Object.selectionForCompositeField "userErrors" [] object____ (Basics.identity >> Decode.list)
