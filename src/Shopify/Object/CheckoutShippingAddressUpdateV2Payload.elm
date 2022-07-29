-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.CheckoutShippingAddressUpdateV2Payload exposing (..)

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


{-| The updated checkout object.
-}
checkout :
    SelectionSet decodesTo Shopify.Object.Checkout
    -> SelectionSet (Maybe decodesTo) Shopify.Object.CheckoutShippingAddressUpdateV2Payload
checkout object____ =
    Object.selectionForCompositeField "checkout" [] object____ (Basics.identity >> Decode.nullable)


{-| The list of errors that occurred from executing the mutation.
-}
checkoutUserErrors :
    SelectionSet decodesTo Shopify.Object.CheckoutUserError
    -> SelectionSet (List decodesTo) Shopify.Object.CheckoutShippingAddressUpdateV2Payload
checkoutUserErrors object____ =
    Object.selectionForCompositeField "checkoutUserErrors" [] object____ (Basics.identity >> Decode.list)


{-| The list of errors that occurred from executing the mutation.
-}
userErrors :
    SelectionSet decodesTo Shopify.Object.UserError
    -> SelectionSet (List decodesTo) Shopify.Object.CheckoutShippingAddressUpdateV2Payload
userErrors object____ =
    Object.selectionForCompositeField "userErrors" [] object____ (Basics.identity >> Decode.list)
