-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.Collection exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Shopify.Enum.ProductCollectionSortKeys
import Shopify.InputObject
import Shopify.Interface
import Shopify.Object
import Shopify.Scalar
import Shopify.ScalarCodecs
import Shopify.Union


type alias DescriptionOptionalArguments =
    { truncateAt : OptionalArgument Int }


{-| Stripped description of the collection, single line with HTML tags removed.

  - truncateAt - Truncates string after the given length.

-}
description :
    (DescriptionOptionalArguments -> DescriptionOptionalArguments)
    -> SelectionSet String Shopify.Object.Collection
description fillInOptionals____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { truncateAt = Absent }

        optionalArgs____ =
            [ Argument.optional "truncateAt" filledInOptionals____.truncateAt Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForField "String" "description" optionalArgs____ Decode.string


{-| The description of the collection, complete with HTML formatting.
-}
descriptionHtml : SelectionSet Shopify.ScalarCodecs.Html Shopify.Object.Collection
descriptionHtml =
    Object.selectionForField "ScalarCodecs.Html" "descriptionHtml" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecHtml |> .decoder)


{-| A human-friendly unique string for the collection automatically generated from its title.
Limit of 255 characters.
-}
handle : SelectionSet String Shopify.Object.Collection
handle =
    Object.selectionForField "String" "handle" [] Decode.string


{-| A globally-unique identifier.
-}
id : SelectionSet Shopify.ScalarCodecs.Id Shopify.Object.Collection
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Image associated with the collection.
-}
image :
    SelectionSet decodesTo Shopify.Object.Image
    -> SelectionSet (Maybe decodesTo) Shopify.Object.Collection
image object____ =
    Object.selectionForCompositeField "image" [] object____ (Basics.identity >> Decode.nullable)


type alias MetafieldRequiredArguments =
    { namespace : String
    , key : String
    }


{-| Returns a metafield found by namespace and key.

  - namespace - A container for a set of metafields.
  - key - The identifier for the metafield.

-}
metafield :
    MetafieldRequiredArguments
    -> SelectionSet decodesTo Shopify.Object.Metafield
    -> SelectionSet (Maybe decodesTo) Shopify.Object.Collection
metafield requiredArgs____ object____ =
    Object.selectionForCompositeField "metafield" [ Argument.required "namespace" requiredArgs____.namespace Encode.string, Argument.required "key" requiredArgs____.key Encode.string ] object____ (Basics.identity >> Decode.nullable)


type alias MetafieldsRequiredArguments =
    { identifiers : List Shopify.InputObject.HasMetafieldsIdentifier }


{-| The metafields associated with the resource matching the supplied list of namespaces and keys.

  - identifiers - The list of metafields to retrieve by namespace and key.

-}
metafields :
    MetafieldsRequiredArguments
    -> SelectionSet decodesTo Shopify.Object.Metafield
    -> SelectionSet (List (Maybe decodesTo)) Shopify.Object.Collection
metafields requiredArgs____ object____ =
    Object.selectionForCompositeField "metafields" [ Argument.required "identifiers" requiredArgs____.identifiers (Shopify.InputObject.encodeHasMetafieldsIdentifier |> Encode.list) ] object____ (Basics.identity >> Decode.nullable >> Decode.list)


{-| The URL used for viewing the resource on the shop's Online Store. Returns `null` if the resource is currently not published to the Online Store sales channel.
-}
onlineStoreUrl : SelectionSet (Maybe Shopify.ScalarCodecs.Url) Shopify.Object.Collection
onlineStoreUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Url)" "onlineStoreUrl" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecUrl |> .decoder |> Decode.nullable)


type alias ProductsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , reverse : OptionalArgument Bool
    , sortKey : OptionalArgument Shopify.Enum.ProductCollectionSortKeys.ProductCollectionSortKeys
    , filters : OptionalArgument (List Shopify.InputObject.ProductFilter)
    }


{-| List of products in the collection.

  - first - Returns up to the first `n` elements from the list.
  - after - Returns the elements that come after the specified cursor.
  - last - Returns up to the last `n` elements from the list.
  - before - Returns the elements that come before the specified cursor.
  - reverse - Reverse the order of the underlying list.
  - sortKey - Sort the underlying list by the given key.
  - filters - Returns a subset of products matching all product filters.

-}
products :
    (ProductsOptionalArguments -> ProductsOptionalArguments)
    -> SelectionSet decodesTo Shopify.Object.ProductConnection
    -> SelectionSet decodesTo Shopify.Object.Collection
products fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, reverse = Absent, sortKey = Absent, filters = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "reverse" filledInOptionals____.reverse Encode.bool, Argument.optional "sortKey" filledInOptionals____.sortKey (Encode.enum Shopify.Enum.ProductCollectionSortKeys.toString), Argument.optional "filters" filledInOptionals____.filters (Shopify.InputObject.encodeProductFilter |> Encode.list) ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "products" optionalArgs____ object____ Basics.identity


{-| The collection's SEO information.
-}
seo :
    SelectionSet decodesTo Shopify.Object.Seo
    -> SelectionSet decodesTo Shopify.Object.Collection
seo object____ =
    Object.selectionForCompositeField "seo" [] object____ Basics.identity


{-| The collection’s name. Limit of 255 characters.
-}
title : SelectionSet String Shopify.Object.Collection
title =
    Object.selectionForField "String" "title" [] Decode.string


{-| The date and time when the collection was last modified.
-}
updatedAt : SelectionSet Shopify.ScalarCodecs.DateTime Shopify.Object.Collection
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)
