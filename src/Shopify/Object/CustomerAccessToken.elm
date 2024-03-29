-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.CustomerAccessToken exposing (..)

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


{-| The customer’s access token.
-}
accessToken : SelectionSet String Shopify.Object.CustomerAccessToken
accessToken =
    Object.selectionForField "String" "accessToken" [] Decode.string


{-| The date and time when the customer access token expires.
-}
expiresAt : SelectionSet Shopify.ScalarCodecs.DateTime Shopify.Object.CustomerAccessToken
expiresAt =
    Object.selectionForField "ScalarCodecs.DateTime" "expiresAt" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)
