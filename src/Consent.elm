port module Consent exposing (..)

import Browser
import Components as C
import Html as H exposing (Attribute, Html)
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
  | Noop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    Loading state ->
      case msg of
        UpdateTime time -> ( Loaded (load time state), Cmd.none )

        _ -> ( model, Cmd.none )

    Loaded state ->
      case msg of
        Noop -> ( model, Cmd.none )

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
    Loading x -> viewOnlyConsentLink
    Loaded { uiState, consent } ->
      case uiState of
        UIClosed -> viewOnlyConsentLink
        UIOpen -> viewOpen
        UIChoose -> viewChoose consent

wrapper : List (Html Msg) -> Html Msg
wrapper content =
  H.div [ A.class "dib"]
    <| List.append
      [ C.consentLink [ E.onClick Open ] ]
      content

viewOnlyConsentLink : Html Msg
viewOnlyConsentLink =
  wrapper []

viewOpen : Html Msg
viewOpen =
  wrapper
    [ H.div
        [ A.class "fixed bottom-0 left-0 black bg-white ba tl pa2" ]
        [ C.h4 [] [ H.text "You are in control of your own cookies" ]
        , C.p """
We use cookies on this site. Some are strictly necessary to run the site while
others like those for measuring how the site is used are optional.
"""
        , H.div [ A.class "flex flex-wrap" ]
            [ primaryButton "Accept All Cookies" (UpdateAndClose chooseAll)
            , secondaryButton "Cookie Settings" Choose
            ]
        ]
    ]

viewChoose: ConsentData -> Html Msg
viewChoose consent =
  wrapper
    [ H.div
        [ A.class "fixed bottom-0 left-0 black bg-white ba tl vh-100"]
        [ H.button
            [ A.class "absolute top-1 right-1 f5 link dim black ph2 pt2 pb1 ba border-box bg-white z-1"
            , E.onClick Close
            ]
            [ H.text "Close" ]
        , H.div
            [ A.class "overflow-scroll vh-100 pa3" ]
            [ C.h3 [] [ H.text "Consent Preferences" ]
            , C.p """
When you visit any website, it may store or retrieve information on your
browser, mostly in the form of cookies. This information might be about you,
your preferences or your device and is mostly used to make the site work as you
expect it to. The information does not usually directly identify you, but it
can give you a more personalised web experience. Because we respect your right
to privacy, you can choose not to allow some types of cookies. Click on the
different category headings to find out more and change our default settings.
"""
            , primaryButton "Allow All" (UpdateAndClose chooseAll)
            , C.h3 [] [ H.text "Manage Consent Preferences" ]
            , H.div
                [ A.class "mv4 flex justify-between items-center" ]
                [ C.h4
                    [ A.class "mv0" ]
                    [ H.text "Strictly Necessary Cookies" ]
                , toggle
                    "input-necessary-cookies"
                    "Strictly Necessary Cookies"
                    (always Noop)
                    True
                    True
                ]
            , C.p """
These cookies are necessary for the website to function and cannot be switched
off in our systems. They are usually only set in response to actions made by
you which amount to a request for services, such as setting your privacy
preferences, adding products to your basket or checking out. You can set your
browser to block or alert you about these cookies, but some parts of the site
will not then work. These cookies do not store any personally identifiable
information.
"""
            , H.div
                [ A.class "mv4 flex justify-between items-center" ]
                [ C.h4
                    [ A.class "mv0" ]
                    [ H.text "Performance Cookies" ]
                , toggle
                    "input-performance-cookies"
                    "Performance Cookies"
                    (always <| Update performanceToggler)
                    False
                    consent.performance
                ]
            , C.p """
These cookies allow us to count visits and traffic sources, so we can measure
and improve the performance of our site. They help us know which pages are the
most and least popular and see how visitors move around the site. All
information these cookies collect is aggregated and therefore anonymous. If you
do not allow these cookies, we will not know when you have visited our site.
"""
            , primaryButton "Save Preferences" Close
            ]
        ]
    ]

-- VIEW COMPONENTS

primaryButton : String -> msg -> Html msg
primaryButton text tagger =
  baseButton
    [ A.class "bn white bg-black"
    , E.onClick tagger
    ]
    text

secondaryButton : String -> msg -> Html msg
secondaryButton text tagger =
  baseButton
    [ A.class "ba black bg-white"
    , E.onClick tagger
    ]
    text

baseButton : List (Attribute msg) -> String ->  Html msg
baseButton attributes text =
  let
    newAttributes =
      List.append
        [ A.class "f5 dim no-underline br-pill ph3 pt2 pb1 mb2 dib mh2" ]
        attributes
  in
    H.button newAttributes [ H.text text ]

toggle : String -> String -> ( Bool -> msg ) -> Bool -> Bool -> Html msg
toggle id label tagger disabled checked =
  let
    labelDisabledClass = if disabled then "bg-moon-gray moon-gray" else "pointer"
    labelCheckedClass = if checked then "bg-black black" else "bg-silver silver"
    nobCheckedStyle = if checked then "translate(100%, 0%)" else "translate(0%, 0%)"
    tickCheckedClass = if checked then "black" else "white"
  in
    H.div
      [ A.class "dib mh2" ]
      [ H.input
          [ A.class "dn"
          , A.id id
          , A.name id
          , A.type_ "checkbox"
          , A.checked checked
          , A.disabled disabled
          , E.onCheck tagger
          ]
          []
      , H.label -- Background
          [ A.class "dib w3 h2 br-pill ba bw1 flex"
          , A.class labelCheckedClass
          , A.class labelDisabledClass
          , A.style "transition" "background .2s cubic-bezier(0.4, 0, 0.4, 1)"
          , A.for id
          ]
          [ H.span -- Nob
              [ A.class "dib w2 h2 br-pill bg-white flex justify-center items-center"
              , A.style "transition" "transform .2s cubic-bezier(0.4, 0, 0.4, 1)"
              , A.style "transform" nobCheckedStyle
              ]
              [ H.span -- Tick
                  [ A.class "db w-25 h-50 br bb bw1 mb1"
                  , A.class tickCheckedClass
                  , A.style "transform" "rotate(45deg)"
                  , A.style "transition" "color .2s cubic-bezier(0.4, 0, 0.4, 1)"
                  ]
                  []
              ]
          , H.span
              [ A.class "dn" ] [ H.text label ]
          ]
      ]
