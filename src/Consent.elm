port module Consent exposing (..)

import Browser
import Components as C
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing (Set)
import Task
import Time

-- PORTS

port consentUpdate : Encode.Value -> Cmd msg

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type UIState = UIClosed | UIOpen | UIChoose

type Model
  = Loading LoadingState
  | Loaded LoadedState

type alias LoadingState = Maybe ConsentData

type alias LoadedState =
  { uiState : UIState
  , consent : ConsentData
  }

type alias ConsentData =
  { dateTime : Time.Posix
  , performance : Bool
  }

createConsentData : Time.Posix -> Bool -> ConsentData
createConsentData dateTime performance =
  { dateTime = dateTime
  , performance = performance
  }

defaultConsent : Time.Posix -> ConsentData
defaultConsent time =
  createConsentData time False

load : Time.Posix -> Maybe ConsentData -> LoadedState
load time loadedConsent =
  case loadedConsent of
    Just consent ->
      { uiState = UIClosed
      , consent = consent |> updateTime time
      }
    Nothing ->
      { uiState = UIOpen
      , consent = defaultConsent time
      }

updateTime : Time.Posix -> ConsentData -> ConsentData
updateTime time consent =
  { consent | dateTime = time }

performanceToggler : ConsentData -> ConsentData
performanceToggler consent =
  { consent | performance = not consent.performance }

chooseAll : ConsentData -> ConsentData
chooseAll consent =
  { consent | performance = True }

init : Maybe String -> ( Model, Cmd Msg)
init flags =
  let
    value = Maybe.withDefault "" flags
      |> decodeConsentData
      |> Result.toMaybe
  in
    ( Loading value
    , Cmd.batch
        [ Task.perform UpdateTime Time.now
        , value |> encodeMaybeConsentData |> consentUpdate
        ]
    )

consentDataDecoder : Decode.Decoder ConsentData
consentDataDecoder =
  Decode.map2 createConsentData
    (Decode.field "dateTime" Decode.int
      |> Decode.map Time.millisToPosix)
    (Decode.field "performance" Decode.bool)

decodeConsentData : String -> Result Decode.Error ConsentData
decodeConsentData value =
  Decode.decodeString consentDataDecoder value

encodeMaybeConsentData : Maybe ConsentData -> Encode.Value
encodeMaybeConsentData data =
  data
    |> Maybe.map encodeConsentData
    |> Maybe.withDefault (Encode.null)

encodeConsentData : ConsentData -> Encode.Value
encodeConsentData data =
  Encode.object
    [ ("dateTime", data.dateTime |> Time.posixToMillis |> Encode.int)
    , ("performance", Encode.bool data.performance)
    ]

-- UPDATE

type Msg
  = Open
  | Choose
  | Update (ConsentData -> ConsentData)
  | UpdateAndClose (ConsentData -> ConsentData)
  | Close
  | UpdateTime Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Loading state ->
      case msg of
        UpdateTime time -> ( Loaded (load time state), Cmd.none )

        _ -> ( model, Cmd.none )

    Loaded state ->
      case msg of
        UpdateTime time ->
          ( Loaded { state | consent = updateTime time state.consent }
          , Cmd.none
          )

        Open ->
          ( Loaded { state | uiState = UIChoose }
          , Cmd.none
          )

        Choose ->
          ( Loaded { state | uiState = UIChoose }
          , Cmd.none
          )

        UpdateAndClose updateConsent ->
          let
            newConsent = updateConsent state.consent
          in
            ( Loaded
                { state
                | uiState = UIClosed
                , consent = newConsent
                }
            , newConsent |> encodeConsentData |> consentUpdate
            )

        Update updateConsent ->
          ( Loaded { state | consent = updateConsent state.consent }
          , Cmd.none
          )

        Close ->
          ( Loaded { state | uiState = UIClosed }
          , state.consent |> encodeConsentData |> consentUpdate
          )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  case model of
    Loading x -> viewLoading
    Loaded { uiState, consent } ->
      case uiState of
        UIClosed -> viewClosed
        UIOpen -> viewOpen
        UIChoose -> viewChoose consent

wrapper : List (Html Msg) -> Html Msg
wrapper content =
  H.div [ A.class "dib"]
    [ C.consentLink [ E.onClick Open ]
    , H.div
        [ A.class "fixed bottom-0 left-0 pa3 black bg-white flex flex-column"]
        content
    ]

viewLoading : Html Msg
viewLoading =
  wrapper [ H.text "Cookie Consent Loading" ]

viewClosed : Html Msg
viewClosed =
  wrapper [ H.text "Cookie Consent Closed" ]

viewOpen : Html Msg
viewOpen =
  wrapper
    [ H.text "Cookie Consent Open"
    , H.button [ E.onClick <| UpdateAndClose chooseAll ] [ H.text "Accept All Cookies" ]
    , H.button [ E.onClick Choose ] [ H.text "Cookie Settings" ]
    , H.button [ E.onClick Close ] [ H.text "Close" ]
    ]

choiceClass : (ConsentData -> Bool) -> ConsentData -> String
choiceClass getter consents =
  if getter consents then
    "bg-silver"
  else
    "bg-black"

viewChoose: ConsentData -> Html Msg
viewChoose consent =
  wrapper
    [ H.text "Cookie Consent Choose"
    , H.button
        [ E.onClick <| UpdateAndClose chooseAll ]
        [ H.text "Accept All" ]
    , H.button
        [ A.class "f6 link bn br-pill ph3 pv2 mb2 dib white bg-silver"
        , A.disabled True
        ]
        [ H.text "Strictle Necessary Cookies" ]
    , H.button
        [ A.class "f6 link dim bn br-pill ph3 pv2 mb2 dib white pointer"
        , A.class <| choiceClass .performance consent
        , E.onClick <| Update performanceToggler
        ]
        [ H.text "Performance Cookies" ]
    , H.button
        [ E.onClick Close ]
        [ H.text "Close" ]
    ]
