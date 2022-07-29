-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.SellingPlanGroupEdge exposing (..)

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


{-| A cursor for use in pagination.
-}
cursor : SelectionSet String Shopify.Object.SellingPlanGroupEdge
cursor =
    Object.selectionForField "String" "cursor" [] Decode.string


{-| The item at the end of SellingPlanGroupEdge.
-}
node :
    SelectionSet decodesTo Shopify.Object.SellingPlanGroup
    -> SelectionSet decodesTo Shopify.Object.SellingPlanGroupEdge
node object____ =
    Object.selectionForCompositeField "node" [] object____ Basics.identity
