-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.CustomerAddressDeletePayload exposing (..)

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


{-| The list of errors that occurred from executing the mutation.
-}
customerUserErrors :
    SelectionSet decodesTo Shopify.Object.CustomerUserError
    -> SelectionSet (List decodesTo) Shopify.Object.CustomerAddressDeletePayload
customerUserErrors object____ =
    Object.selectionForCompositeField "customerUserErrors" [] object____ (Basics.identity >> Decode.list)


{-| ID of the deleted customer address.
-}
deletedCustomerAddressId : SelectionSet (Maybe String) Shopify.Object.CustomerAddressDeletePayload
deletedCustomerAddressId =
    Object.selectionForField "(Maybe String)" "deletedCustomerAddressId" [] (Decode.string |> Decode.nullable)


{-| The list of errors that occurred from executing the mutation.
-}
userErrors :
    SelectionSet decodesTo Shopify.Object.UserError
    -> SelectionSet (List decodesTo) Shopify.Object.CustomerAddressDeletePayload
userErrors object____ =
    Object.selectionForCompositeField "userErrors" [] object____ (Basics.identity >> Decode.list)
