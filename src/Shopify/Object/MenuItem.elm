-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.MenuItem exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Shopify.Enum.MenuItemType
import Shopify.InputObject
import Shopify.Interface
import Shopify.Object
import Shopify.Scalar
import Shopify.ScalarCodecs
import Shopify.Union


{-| A globally-unique identifier.
-}
id : SelectionSet Shopify.ScalarCodecs.Id Shopify.Object.MenuItem
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The menu item's child items.
-}
items :
    SelectionSet decodesTo Shopify.Object.MenuItem
    -> SelectionSet (List decodesTo) Shopify.Object.MenuItem
items object____ =
    Object.selectionForCompositeField "items" [] object____ (Basics.identity >> Decode.list)


{-| The ID of the linked resource.
-}
resourceId : SelectionSet (Maybe Shopify.ScalarCodecs.Id) Shopify.Object.MenuItem
resourceId =
    Object.selectionForField "(Maybe ScalarCodecs.Id)" "resourceId" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecId |> .decoder |> Decode.nullable)


{-| The menu item's tags to filter a collection.
-}
tags : SelectionSet (List String) Shopify.Object.MenuItem
tags =
    Object.selectionForField "(List String)" "tags" [] (Decode.string |> Decode.list)


{-| The menu item's title.
-}
title : SelectionSet String Shopify.Object.MenuItem
title =
    Object.selectionForField "String" "title" [] Decode.string


{-| The menu item's type.
-}
type_ : SelectionSet Shopify.Enum.MenuItemType.MenuItemType Shopify.Object.MenuItem
type_ =
    Object.selectionForField "Enum.MenuItemType.MenuItemType" "type" [] Shopify.Enum.MenuItemType.decoder


{-| The menu item's URL.
-}
url : SelectionSet (Maybe Shopify.ScalarCodecs.Url) Shopify.Object.MenuItem
url =
    Object.selectionForField "(Maybe ScalarCodecs.Url)" "url" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecUrl |> .decoder |> Decode.nullable)
