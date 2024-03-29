-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.ShippingRate exposing (..)

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


{-| Human-readable unique identifier for this shipping rate.
-}
handle : SelectionSet String Shopify.Object.ShippingRate
handle =
    Object.selectionForField "String" "handle" [] Decode.string


{-| Price of this shipping rate.
-}
price : SelectionSet Shopify.ScalarCodecs.Money Shopify.Object.ShippingRate
price =
    Object.selectionForField "ScalarCodecs.Money" "price" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecMoney |> .decoder)


{-| Price of this shipping rate.
-}
priceV2 :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet decodesTo Shopify.Object.ShippingRate
priceV2 object____ =
    Object.selectionForCompositeField "priceV2" [] object____ Basics.identity


{-| Title of this shipping rate.
-}
title : SelectionSet String Shopify.Object.ShippingRate
title =
    Object.selectionForField "String" "title" [] Decode.string
