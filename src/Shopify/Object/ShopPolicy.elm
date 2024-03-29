-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.ShopPolicy exposing (..)

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


{-| Policy text, maximum size of 64kb.
-}
body : SelectionSet String Shopify.Object.ShopPolicy
body =
    Object.selectionForField "String" "body" [] Decode.string


{-| Policy’s handle.
-}
handle : SelectionSet String Shopify.Object.ShopPolicy
handle =
    Object.selectionForField "String" "handle" [] Decode.string


{-| A globally-unique identifier.
-}
id : SelectionSet Shopify.ScalarCodecs.Id Shopify.Object.ShopPolicy
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Policy’s title.
-}
title : SelectionSet String Shopify.Object.ShopPolicy
title =
    Object.selectionForField "String" "title" [] Decode.string


{-| Public URL to the policy.
-}
url : SelectionSet Shopify.ScalarCodecs.Url Shopify.Object.ShopPolicy
url =
    Object.selectionForField "ScalarCodecs.Url" "url" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecUrl |> .decoder)
