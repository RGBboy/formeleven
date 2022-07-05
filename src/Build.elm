port module Build exposing (..)

import Dict exposing (Dict)
import ElmHtml.InternalTypes exposing (decodeElmHtml)
import ElmHtml.ToString exposing (nodeToStringWithOptions, defaultFormatOptions)
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages
import Platform

asJsonString : Html msg -> String
asJsonString x = "REPLACE_ME_WITH_JSON_STRINGIFY"

options = { defaultFormatOptions | newLines = False, indent = 0 }

htmlToString : Html msg -> String
htmlToString html =
  case Decode.decodeString (decodeElmHtml (\_ _ -> Decode.succeed ())) (asJsonString html) of
    Err error -> Decode.errorToString error
    Ok str -> nodeToStringWithOptions options str

encodePage : (String, Pages.Page msg) -> (String, Encode.Value)
encodePage (path, { title, description, body } ) =
  (path
  , Encode.object
      [ ( "title", Encode.string title )
      , ( "description", Encode.string description )
      , ( "body", htmlToString body |> Encode.string )
      ]
  )

encodedPages : Pages.DataModel -> Encode.Value
encodedPages data =
  Pages.generate data
    |> List.map encodePage
    |> Encode.object

init : Pages.DataModel -> ( (), Cmd msg )
init data =
  ((), encodedPages data |> htmlOut )

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
