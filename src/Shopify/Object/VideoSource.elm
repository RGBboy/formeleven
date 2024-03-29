-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.VideoSource exposing (..)

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


{-| The format of the video source.
-}
format : SelectionSet String Shopify.Object.VideoSource
format =
    Object.selectionForField "String" "format" [] Decode.string


{-| The height of the video.
-}
height : SelectionSet Int Shopify.Object.VideoSource
height =
    Object.selectionForField "Int" "height" [] Decode.int


{-| The video MIME type.
-}
mimeType : SelectionSet String Shopify.Object.VideoSource
mimeType =
    Object.selectionForField "String" "mimeType" [] Decode.string


{-| The URL of the video.
-}
url : SelectionSet String Shopify.Object.VideoSource
url =
    Object.selectionForField "String" "url" [] Decode.string


{-| The width of the video.
-}
width : SelectionSet Int Shopify.Object.VideoSource
width =
    Object.selectionForField "Int" "width" [] Decode.int
