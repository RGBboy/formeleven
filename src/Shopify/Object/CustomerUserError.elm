-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.CustomerUserError exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Shopify.Enum.CustomerErrorCode
import Shopify.InputObject
import Shopify.Interface
import Shopify.Object
import Shopify.Scalar
import Shopify.ScalarCodecs
import Shopify.Union


{-| The error code.
-}
code : SelectionSet (Maybe Shopify.Enum.CustomerErrorCode.CustomerErrorCode) Shopify.Object.CustomerUserError
code =
    Object.selectionForField "(Maybe Enum.CustomerErrorCode.CustomerErrorCode)" "code" [] (Shopify.Enum.CustomerErrorCode.decoder |> Decode.nullable)


{-| The path to the input field that caused the error.
-}
field : SelectionSet (Maybe (List String)) Shopify.Object.CustomerUserError
field =
    Object.selectionForField "(Maybe (List String))" "field" [] (Decode.string |> Decode.list |> Decode.nullable)


{-| The error message.
-}
message : SelectionSet String Shopify.Object.CustomerUserError
message =
    Object.selectionForField "String" "message" [] Decode.string
