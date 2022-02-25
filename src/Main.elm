module Main exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url.Builder

url : String -> String
url email = Url.Builder.crossOrigin
  "https://script.google.com"
  [ "macros"
  , "s"
  , "AKfycbzVynpp3rXpw94dkIMZzKMeH1mkOHfBx7P8R-5OHehKIOdNSPANb0343naAvH8ekFgS"
  , "exec"
  ]
  [ Url.Builder.string "email" email
  , Url.Builder.string "action" "Subscribe"
  ]

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type FormState = NotSent | Sending | Success | Failure

type alias Model =
  { email : String
  , state : FormState
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { email = ""
    , state = NotSent
    }
  , Cmd.none
  )

type Msg
  = Update String
  | Submit
  | Response (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Update value ->
      ( { model | email = value }
      , Cmd.none
      )
    Submit ->
      ( { model | state = Sending }
      , postForm model.email
      )
    Response result ->
      case result of
        Ok message ->
          ( { model | state = Success }
          , Cmd.none
          )
        Err _ ->
          ( { model | state = Failure }
          , Cmd.none
          )

-- Note: This makes a GET request with data because that is what Google wants
postForm : String -> Cmd Msg
postForm email =
  Http.request
    { method = "GET"
    , headers = []
    , url = url email
    , body = Http.emptyBody
    , expect = Http.expectJson Response (Decode.string)
    , timeout = Nothing
    , tracker = Nothing
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  case model.state of
    NotSent ->
      H.div []
        [ H.input
          [ A.type_ "Text"
          , A.placeholder "Email"
          , A.value model.email
          , E.onInput Update
          ] []
        , H.button [ E.onClick Submit ] [ H.text "Sign Up" ]
        ]
    Sending ->
      H.div [] [ H.text "Sending" ]
    Success ->
      H.div [] [ H.text "Success" ]
    Failure ->
      H.div [] [ H.text "Failure" ]
