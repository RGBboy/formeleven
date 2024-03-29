-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.Currency exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Shopify.Enum.CurrencyCode
import Shopify.InputObject
import Shopify.Interface
import Shopify.Object
import Shopify.Scalar
import Shopify.ScalarCodecs
import Shopify.Union


{-| The ISO code of the currency.
-}
isoCode : SelectionSet Shopify.Enum.CurrencyCode.CurrencyCode Shopify.Object.Currency
isoCode =
    Object.selectionForField "Enum.CurrencyCode.CurrencyCode" "isoCode" [] Shopify.Enum.CurrencyCode.decoder


{-| The name of the currency.
-}
name : SelectionSet String Shopify.Object.Currency
name =
    Object.selectionForField "String" "name" [] Decode.string


{-| The symbol of the currency.
-}
symbol : SelectionSet String Shopify.Object.Currency
symbol =
    Object.selectionForField "String" "symbol" [] Decode.string
