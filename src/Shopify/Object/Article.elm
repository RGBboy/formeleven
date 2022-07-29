-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.Article exposing (..)

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


{-| The article's author.
-}
author :
    SelectionSet decodesTo Shopify.Object.ArticleAuthor
    -> SelectionSet decodesTo Shopify.Object.Article
author object____ =
    Object.selectionForCompositeField "author" [] object____ Basics.identity


{-| The article's author.
-}
authorV2 :
    SelectionSet decodesTo Shopify.Object.ArticleAuthor
    -> SelectionSet (Maybe decodesTo) Shopify.Object.Article
authorV2 object____ =
    Object.selectionForCompositeField "authorV2" [] object____ (Basics.identity >> Decode.nullable)


{-| The blog that the article belongs to.
-}
blog :
    SelectionSet decodesTo Shopify.Object.Blog
    -> SelectionSet decodesTo Shopify.Object.Article
blog object____ =
    Object.selectionForCompositeField "blog" [] object____ Basics.identity


type alias CommentsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , reverse : OptionalArgument Bool
    }


{-| List of comments posted on the article.

  - first - Returns up to the first `n` elements from the list.
  - after - Returns the elements that come after the specified cursor.
  - last - Returns up to the last `n` elements from the list.
  - before - Returns the elements that come before the specified cursor.
  - reverse - Reverse the order of the underlying list.

-}
comments :
    (CommentsOptionalArguments -> CommentsOptionalArguments)
    -> SelectionSet decodesTo Shopify.Object.CommentConnection
    -> SelectionSet decodesTo Shopify.Object.Article
comments fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, reverse = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "reverse" filledInOptionals____.reverse Encode.bool ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "comments" optionalArgs____ object____ Basics.identity


type alias ContentOptionalArguments =
    { truncateAt : OptionalArgument Int }


{-| Stripped content of the article, single line with HTML tags removed.

  - truncateAt - Truncates string after the given length.

-}
content :
    (ContentOptionalArguments -> ContentOptionalArguments)
    -> SelectionSet String Shopify.Object.Article
content fillInOptionals____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { truncateAt = Absent }

        optionalArgs____ =
            [ Argument.optional "truncateAt" filledInOptionals____.truncateAt Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForField "String" "content" optionalArgs____ Decode.string


{-| The content of the article, complete with HTML formatting.
-}
contentHtml : SelectionSet Shopify.ScalarCodecs.Html Shopify.Object.Article
contentHtml =
    Object.selectionForField "ScalarCodecs.Html" "contentHtml" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecHtml |> .decoder)


type alias ExcerptOptionalArguments =
    { truncateAt : OptionalArgument Int }


{-| Stripped excerpt of the article, single line with HTML tags removed.

  - truncateAt - Truncates string after the given length.

-}
excerpt :
    (ExcerptOptionalArguments -> ExcerptOptionalArguments)
    -> SelectionSet (Maybe String) Shopify.Object.Article
excerpt fillInOptionals____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { truncateAt = Absent }

        optionalArgs____ =
            [ Argument.optional "truncateAt" filledInOptionals____.truncateAt Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForField "(Maybe String)" "excerpt" optionalArgs____ (Decode.string |> Decode.nullable)


{-| The excerpt of the article, complete with HTML formatting.
-}
excerptHtml : SelectionSet (Maybe Shopify.ScalarCodecs.Html) Shopify.Object.Article
excerptHtml =
    Object.selectionForField "(Maybe ScalarCodecs.Html)" "excerptHtml" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecHtml |> .decoder |> Decode.nullable)


{-| A human-friendly unique string for the Article automatically generated from its title.
-}
handle : SelectionSet String Shopify.Object.Article
handle =
    Object.selectionForField "String" "handle" [] Decode.string


{-| A globally-unique identifier.
-}
id : SelectionSet Shopify.ScalarCodecs.Id Shopify.Object.Article
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The image associated with the article.
-}
image :
    SelectionSet decodesTo Shopify.Object.Image
    -> SelectionSet (Maybe decodesTo) Shopify.Object.Article
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
    -> SelectionSet (Maybe decodesTo) Shopify.Object.Article
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
    -> SelectionSet (List (Maybe decodesTo)) Shopify.Object.Article
metafields requiredArgs____ object____ =
    Object.selectionForCompositeField "metafields" [ Argument.required "identifiers" requiredArgs____.identifiers (Shopify.InputObject.encodeHasMetafieldsIdentifier |> Encode.list) ] object____ (Basics.identity >> Decode.nullable >> Decode.list)


{-| The URL used for viewing the resource on the shop's Online Store. Returns `null` if the resource is currently not published to the Online Store sales channel.
-}
onlineStoreUrl : SelectionSet (Maybe Shopify.ScalarCodecs.Url) Shopify.Object.Article
onlineStoreUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Url)" "onlineStoreUrl" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecUrl |> .decoder |> Decode.nullable)


{-| The date and time when the article was published.
-}
publishedAt : SelectionSet Shopify.ScalarCodecs.DateTime Shopify.Object.Article
publishedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "publishedAt" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The article’s SEO information.
-}
seo :
    SelectionSet decodesTo Shopify.Object.Seo
    -> SelectionSet (Maybe decodesTo) Shopify.Object.Article
seo object____ =
    Object.selectionForCompositeField "seo" [] object____ (Basics.identity >> Decode.nullable)


{-| A categorization that a article can be tagged with.
-}
tags : SelectionSet (List String) Shopify.Object.Article
tags =
    Object.selectionForField "(List String)" "tags" [] (Decode.string |> Decode.list)


{-| The article’s name.
-}
title : SelectionSet String Shopify.Object.Article
title =
    Object.selectionForField "String" "title" [] Decode.string
