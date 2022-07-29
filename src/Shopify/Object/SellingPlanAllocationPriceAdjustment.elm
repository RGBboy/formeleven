-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.SellingPlanAllocationPriceAdjustment exposing (..)

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


{-| The price of the variant when it's purchased without a selling plan for the same number of deliveries. For example, if a customer purchases 6 deliveries of $10.00 granola separately, then the price is 6 x $10.00 = $60.00.
-}
compareAtPrice :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet decodesTo Shopify.Object.SellingPlanAllocationPriceAdjustment
compareAtPrice object____ =
    Object.selectionForCompositeField "compareAtPrice" [] object____ Basics.identity


{-| The effective price for a single delivery. For example, for a prepaid subscription plan that includes 6 deliveries at the price of $48.00, the per delivery price is $8.00.
-}
perDeliveryPrice :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet decodesTo Shopify.Object.SellingPlanAllocationPriceAdjustment
perDeliveryPrice object____ =
    Object.selectionForCompositeField "perDeliveryPrice" [] object____ Basics.identity


{-| The price of the variant when it's purchased with a selling plan For example, for a prepaid subscription plan that includes 6 deliveries of $10.00 granola, where the customer gets 20% off, the price is 6 x $10.00 x 0.80 = $48.00.
-}
price :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet decodesTo Shopify.Object.SellingPlanAllocationPriceAdjustment
price object____ =
    Object.selectionForCompositeField "price" [] object____ Basics.identity


{-| The resulting price per unit for the variant associated with the selling plan. If the variant isn't sold by quantity or measurement, then this field returns `null`.
-}
unitPrice :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet (Maybe decodesTo) Shopify.Object.SellingPlanAllocationPriceAdjustment
unitPrice object____ =
    Object.selectionForCompositeField "unitPrice" [] object____ (Basics.identity >> Decode.nullable)
