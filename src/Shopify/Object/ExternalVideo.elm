-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.ExternalVideo exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Shopify.Enum.MediaContentType
import Shopify.Enum.MediaHost
import Shopify.InputObject
import Shopify.Interface
import Shopify.Object
import Shopify.Scalar
import Shopify.ScalarCodecs
import Shopify.Union


{-| A word or phrase to share the nature or contents of a media.
-}
alt : SelectionSet (Maybe String) Shopify.Object.ExternalVideo
alt =
    Object.selectionForField "(Maybe String)" "alt" [] (Decode.string |> Decode.nullable)


{-| The embed URL of the video for the respective host.
-}
embedUrl : SelectionSet Shopify.ScalarCodecs.Url Shopify.Object.ExternalVideo
embedUrl =
    Object.selectionForField "ScalarCodecs.Url" "embedUrl" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecUrl |> .decoder)


{-| The URL.
-}
embeddedUrl : SelectionSet Shopify.ScalarCodecs.Url Shopify.Object.ExternalVideo
embeddedUrl =
    Object.selectionForField "ScalarCodecs.Url" "embeddedUrl" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecUrl |> .decoder)


{-| The host of the external video.
-}
host : SelectionSet Shopify.Enum.MediaHost.MediaHost Shopify.Object.ExternalVideo
host =
    Object.selectionForField "Enum.MediaHost.MediaHost" "host" [] Shopify.Enum.MediaHost.decoder


{-| A globally-unique identifier.
-}
id : SelectionSet Shopify.ScalarCodecs.Id Shopify.Object.ExternalVideo
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The media content type.
-}
mediaContentType : SelectionSet Shopify.Enum.MediaContentType.MediaContentType Shopify.Object.ExternalVideo
mediaContentType =
    Object.selectionForField "Enum.MediaContentType.MediaContentType" "mediaContentType" [] Shopify.Enum.MediaContentType.decoder


{-| The origin URL of the video on the respective host.
-}
originUrl : SelectionSet Shopify.ScalarCodecs.Url Shopify.Object.ExternalVideo
originUrl =
    Object.selectionForField "ScalarCodecs.Url" "originUrl" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecUrl |> .decoder)


{-| The preview image for the media.
-}
previewImage :
    SelectionSet decodesTo Shopify.Object.Image
    -> SelectionSet (Maybe decodesTo) Shopify.Object.ExternalVideo
previewImage object____ =
    Object.selectionForCompositeField "previewImage" [] object____ (Basics.identity >> Decode.nullable)
