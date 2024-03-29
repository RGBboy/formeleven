-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.SellingPlanGroupConnection exposing (..)

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


{-| A list of edges.
-}
edges :
    SelectionSet decodesTo Shopify.Object.SellingPlanGroupEdge
    -> SelectionSet (List decodesTo) Shopify.Object.SellingPlanGroupConnection
edges object____ =
    Object.selectionForCompositeField "edges" [] object____ (Basics.identity >> Decode.list)


{-| A list of the nodes contained in SellingPlanGroupEdge.
-}
nodes :
    SelectionSet decodesTo Shopify.Object.SellingPlanGroup
    -> SelectionSet (List decodesTo) Shopify.Object.SellingPlanGroupConnection
nodes object____ =
    Object.selectionForCompositeField "nodes" [] object____ (Basics.identity >> Decode.list)


{-| Information to aid in pagination.
-}
pageInfo :
    SelectionSet decodesTo Shopify.Object.PageInfo
    -> SelectionSet decodesTo Shopify.Object.SellingPlanGroupConnection
pageInfo object____ =
    Object.selectionForCompositeField "pageInfo" [] object____ Basics.identity
