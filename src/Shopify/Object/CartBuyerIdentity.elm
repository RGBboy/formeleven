-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.CartBuyerIdentity exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Shopify.Enum.CountryCode
import Shopify.InputObject
import Shopify.Interface
import Shopify.Object
import Shopify.Scalar
import Shopify.ScalarCodecs
import Shopify.Union


{-| The country where the buyer is located.
-}
countryCode : SelectionSet (Maybe Shopify.Enum.CountryCode.CountryCode) Shopify.Object.CartBuyerIdentity
countryCode =
    Object.selectionForField "(Maybe Enum.CountryCode.CountryCode)" "countryCode" [] (Shopify.Enum.CountryCode.decoder |> Decode.nullable)


{-| The customer account associated with the cart.
-}
customer :
    SelectionSet decodesTo Shopify.Object.Customer
    -> SelectionSet (Maybe decodesTo) Shopify.Object.CartBuyerIdentity
customer object____ =
    Object.selectionForCompositeField "customer" [] object____ (Basics.identity >> Decode.nullable)


{-| The email address of the buyer that is interacting with the cart.
-}
email : SelectionSet (Maybe String) Shopify.Object.CartBuyerIdentity
email =
    Object.selectionForField "(Maybe String)" "email" [] (Decode.string |> Decode.nullable)


{-| The phone number of the buyer that is interacting with the cart.
-}
phone : SelectionSet (Maybe String) Shopify.Object.CartBuyerIdentity
phone =
    Object.selectionForField "(Maybe String)" "phone" [] (Decode.string |> Decode.nullable)
