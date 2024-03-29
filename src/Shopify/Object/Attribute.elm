-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.Attribute exposing (..)

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


{-| Key or name of the attribute.
-}
key : SelectionSet String Shopify.Object.Attribute
key =
    Object.selectionForField "String" "key" [] Decode.string


{-| Value of the attribute.
-}
value : SelectionSet (Maybe String) Shopify.Object.Attribute
value =
    Object.selectionForField "(Maybe String)" "value" [] (Decode.string |> Decode.nullable)
