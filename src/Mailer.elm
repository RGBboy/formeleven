module Mailer exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Result.Extra as Result
import Url.Builder
import Validate exposing (Valid, Validator)

url : String
url = Url.Builder.crossOrigin
  "https://script.google.com"
  [ "macros"
  , "s"
  , "AKfycbyGrqDjb5Fl7Fh08ov-ifqyK91sgWXmPhj3YHEA3_3KqVrMQ32ED8B9zRfl2Z4lwkZq"
  , "exec"
  ]
  []

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type RequestSuccess = RequestSuccess

type FormState
  = NotSent
  | Attempted
  | Sending
  | Success
  | Failure

type alias Model =
  { email : String
  , validation : Result (List String) (Valid String)
  , state : FormState
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { email = ""
    , validation = validateEmail ""
    , state = NotSent
    }
  , Cmd.none
  )

type Msg
  = Update String
  | Submit
  | Response (Result Http.Error RequestSuccess)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Update value ->
      ( { model | email = value, validation = validateEmail value }
      , Cmd.none
      )
    Submit ->
      if Result.isOk model.validation
        && model.state /= Sending
        then
        ( { model | state = Sending }
        , postForm model.email
        )
      else
        ( { model | state = Attempted }
        , Cmd.none
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


validateEmail : String -> Result (List String) (Valid String)
validateEmail email =
  Validate.validate emailValidator email

emailValidator : Validator String String
emailValidator =
  Validate.firstError
    [ Validate.ifInvalidEmail identity (\_ -> "Please enter a valid email address.")
    , Validate.ifBlank identity "Please enter a valid email address."
    ]

createRequestBody : String -> Encode.Value
createRequestBody email =
  Encode.object
  [ ("action", Encode.string "Subscribe")
  , ( "email", Encode.string email )
  ]

decodeResponse : Decode.Decoder RequestSuccess
decodeResponse =
  Decode.field "result" Decode.string
    |> Decode.andThen decodeSuccess

decodeSuccess : String -> Decode.Decoder RequestSuccess
decodeSuccess value =
  case value of
    "success" -> Decode.succeed RequestSuccess
    _ -> Decode.fail "Server error"

postForm : String -> Cmd Msg
postForm email =
  Http.post
    { url = url
    -- For POST requests to Google App Scripts "text/plain" needs to be used for CORS
    , body = Http.stringBody "text/plain" (Encode.encode 0 <| createRequestBody email)
    , expect = Http.expectJson Response decodeResponse
    }



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



submitButtonView : Bool -> Html Msg
submitButtonView isDisabled =
  let
    buttonText =
      if isDisabled then
        "Sending..."
      else
        "Subscribe"
  in
    H.button
      [ A.classList
        [ ("f6 f5-l button-reset ba b--mid-gray fl lh-solid pv3 tc bg-animate white bg-mid-gray w-100 w-25-m w-20-l br2-ns br--right-ns", True)
        , ("hover-bg-gray pointer", not isDisabled)
        ]
      , A.disabled isDisabled
      ]
      [ H.text buttonText ]

errorView : String -> Html msg
errorView error =
  H.div
    [ A.class "f6 f5-l black-80 bg-washed-red ba b--light-red br2-ns pa3 lh-solid w-100 mv2" ]
    [ H.text error ]

emailInputView : String -> List String -> Bool -> Html Msg
emailInputView email errors isDisabled =
  let
    children =
      [ H.label
        [ A.class "clip" ]
        [ H.text "Email Address" ]
      , H.input
        [ A.classList
          [ ("f6 f5-l input-reset ba black-80 pa3 lh-solid w-100 br2-ns br--left-ns", True)
          , ("b--light-red", not <| List.isEmpty errors)
          , ("b--white bg-white", not isDisabled)
          , ("b--moon-gray bg-moon-gray", isDisabled)
          ]
        , A.type_ "Text"
        , A.placeholder "Email Address"
        , A.value email
        , A.disabled isDisabled
        , E.onInput Update
        ] []
      ] ++ (List.map errorView errors)
  in
    H.div
      [ A.class "w-100 w-75-m w-80-l fl" ]
      children

formView : Model -> Bool -> Html Msg
formView model isSending =
  let
    showErrors = model.state == Attempted
    errors =
      if showErrors then
        model.validation
          |> Result.mapBoth (List.map identity) (always [])
          |> Result.merge
      else
        []
  in
    H.form
      [ A.class ""
      , E.onSubmit Submit
      ]
      [ H.fieldset
        [ A.class "cf bn ma0 pa0"
        ]
        [ H.legend
          [ A.class "f5 f4-ns fw2 mb3 pa0"]
          [ H.text "Join the mailing list to be the first to know about sales and product updates." ]
        , H.small
          [ A.class "f6 fw2"]
          [ H.text "We don't send many emails and you can unsubscribe at any time. More information can be found in our "
          , H.a
            [ A.class "link white underline dim"
            , A.href "/terms#privacy"
            ]
            [ H.text "privacy policy" ]
          , H.text "."
          ]
        , H.div
          [ A.class "cf mt3" ]
          [ emailInputView model.email errors isSending
          , submitButtonView isSending
          ]
        ]
      ]

view : Model -> Html Msg
view model =
  let
    child =
      case model.state of
        NotSent ->
          formView model False
        Attempted ->
          formView model False
        Sending ->
          formView model True
        Success ->
          H.p
            [ A.class "f5 f4-ns fw2 mb3 pa0"]
            [ H.text "You've successfully subscribed to be the first to know about sales and product updates. Thanks!" ]
        Failure ->
          H.p
            [ A.class "f5 f4-ns fw2 mb3 pa0"]
            [ H.text "Something unexpected has gone wrong. If you would still like to subscribe you can email us at "
            , H.a
              [ A.class "white tl link dim dib ul"
              , A.href "mailto:hello@formeleven.com"
              ]
              [ H.text "hello@formeleven.com" ]
            , H.text "."
            ]
  in
    H.div
      [ A.class "mw6 center" ]
      [ H.h2
        [ A.class "f2 fw2 mt4 mt5-ns mb4 bb b--silver" ]
        [ H.text "Mailing List" ]
      , child
      ]
