-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.SellingPlanFixedPriceAdjustment exposing (..)

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


{-| A new price of the variant when it's purchased with the selling plan.
-}
price :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet decodesTo Shopify.Object.SellingPlanFixedPriceAdjustment
price object____ =
    Object.selectionForCompositeField "price" [] object____ Basics.identity
