port module Build exposing (..)

import Dict exposing (Dict)
import ElmHtml.InternalTypes exposing (decodeElmHtml)
import ElmHtml.ToString exposing (nodeToStringWithOptions, defaultFormatOptions)
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages exposing (pages)
import Platform

asJsonString : Html msg -> String
asJsonString x = "REPLACE_ME_WITH_JSON_STRINGIFY"

options = { defaultFormatOptions | newLines = False, indent = 0 }

htmlToString : Html msg -> String
htmlToString html =
  case Decode.decodeString (decodeElmHtml (\_ _ -> Decode.succeed ())) (asJsonString html) of
    Err error -> Decode.errorToString error
    Ok str -> nodeToStringWithOptions options str

encodePage : (String, Html msg) -> (String, Encode.Value)
encodePage (path, page) =
  (path, htmlToString page |> Encode.string)

encodedPages : Encode.Value
encodedPages =
  Encode.object (List.map encodePage pages)

init : () -> ( (), Cmd msg )
init flags =
  ((), htmlOut encodedPages )

update : msg -> model -> ( model, Cmd msg )
update _ model =
  (model, Cmd.none)

subscriptions : model -> Sub msg
subscriptions _ = Sub.none

main = Platform.worker
  { init = init
  , update = update
  , subscriptions = subscriptions
  }

port htmlOut : Encode.Value -> Cmd msg
