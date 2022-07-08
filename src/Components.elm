module Components exposing (..)

import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Json.Encode as Encode



layout : List (Html msg) -> Html msg
layout content =
  body
    [ header
    , H.node "main" [ A.class "w-100 mw8 mb5 center" ] content
    , footer
    ]

body : List (Html msg) -> Html msg
body content =
  H.node "body"
    [ A.class "w-100 bg-white sans-serif dark-grey"
    , A.style "font-family" "'Josefin Sans', sans-serif;"
    ]
    content

header : Html msg
header =
  H.header [ A.class "w-100 mw8 center mv4" ]
    [ H.a
        [ A.class "flex justify-center link dim black"
        , A.href "/"
        ]
        [ H.img
            [ A.class "h3 h4-l w-auto"
            , A.src "/frontend/img/logo-512x512.png"
            , A.width 512
            , A.height 512
            ] []
        , H.h1 [ A.class "f3 f2-l fw3 ttu self-end mv0" ]
            [ H.text "Form "
            , H.br [] []
            , H.text "Eleven"
            ]
        ]
    , H.h2 [ A.class "f4 fw2 tc mt3 mb0" ] [ H.text "Combining digital fabrication, handmade ceramics and luminaires." ]
    , H.p [ A.class "f5 fw2 tc" ] [ H.text "Deptford, London, UK." ]
    , H.div [ A.class "pv2 tc" ]
        [ H.a
            [ A.class "link hover-silver near-black dib h2 w2 mh2"
            , A.href "https://www.instagram.com/formeleven/"
            , A.title "Instagram"
            ]
            [ iconInstagram ]
        ]
    ]

footer : Html msg
footer =
  H.footer [ A.class "w-100 bg-black white pt1 pb3 ph4 ph2-ns" ]
    [ H.section [ A.id "newsletter" ] []
    , H.section [ A.class "mw6 mv4 center" ]
        [ H.h2 [ A.class "f2 fw2 mt4 mt5-ns mb4 bb b--silver" ] [ H.text "Get In Touch" ]
        , H.div [ A.class "flex flex-wrap flex-row-reverse justify-center-ns justify-end" ]
            [ H.div [ A.class "w-100 w-50-ns" ]
                [ H.div [ A.class "pv2" ]
                    [ H.a
                        [ A.class "white f4 tl link dim dib"
                        , A.href "mailto:hello@formeleven.com"
                        ]
                        [ H.text "hello@formeleven.com" ]
                    , H.div [ A.class "pv2" ]
                        [ H.a
                          [ A.class "link hover-silver white dib h2 w2 mh2"
                          , A.href "https://www.instagram.com/formeleven/"
                          , A.title "Instagram"
                          ]
                          [ iconInstagram ]
                        ]
                    ]
                ]
            , H.article [ A.class "vcard w-100 w-50-ns pv2" ]
                [ H.span [ A.class "adr f5" ]
                    [ H.span [ A.class "street-address" ]
                        [ H.span [ A.class "f4 db" ] [ H.text "Studio AR06" ]
                        , H.span [ A.class "db" ] [ H.text "Second Floor Studios and Arts" ]
                        , H.span [ A.class "db" ] [ H.text "Arbor House, Moulding Lane" ]
                        ]
                    , H.span [ A.class "postal-code db" ] [ H.text "SE14 6BS" ]
                    , H.span [ A.class "db" ]
                        [ H.span [ A.class "region" ] [ H.text "London" ]
                        , H.text ", "
                        , H.span [ A.class "country-name" ] [ H.text "UK" ]
                        ]
                    ]
                ]
            ]
        ]
    , H.small [ A.class "f6 db tc-ns" ]
        [ H.text "© 2022 "
        , H.b [ A.class "ttu" ] [ H.text "Form Eleven" ]
        , H.text " All Rights Reserved | "
        , consentLink []
        , H.text " | "
        , H.a
            [ A.class "link white underline dim"
            , A.href "/frontend/terms.html"
            , A.title "Terms and Conditions"
            ]
            [ H.text "Terms & Conditions"]
        ]
    ]

-- TILES
tileLayout : List (Html msg) -> Html msg
tileLayout =
  H.div [ A.class "flex flex-wrap mv3" ]

tileExtra : List (Attribute msg) -> List (Html msg) -> Html msg
tileExtra attributes =
  A.class "w-100 w-third-m w-25-l pa2" :: attributes
    |> H.div

tileFirst : List (Html msg) -> Html msg
tileFirst =
  tileExtra [ A.class "tr-ns self-end-ns" ]

tileImage : String -> Html msg
tileImage url =
  tileExtra []
    [ H.img
      [ A.class "db mw-100 h-auto bg-light-gray"
      , A.src url
      , A.width 512
      , A.height 512
      ]
      []
    ]

tileInfo : String -> String -> Html msg
tileInfo title description =
  H.dl [ A.class "ma0 f6"]
    [ H.dt [ A.class "clip" ] [ H.text "Title"]
    , H.dd [ A.class "f3 ma0" ] [ H.text title ]
    , H.dt [ A.class "clip" ] [ H.text "Description"]
    , H.dd [ A.class "f5 fw2 ma0" ] [ H.text description ]
    ]

tileSpacerL : Html msg
tileSpacerL =
  H.div [ A.class "w-0 w-25-l pa2-l db" ] []

tileSpacerM : Html msg
tileSpacerM =
  H.div [ A.class "w-0 w-third-m w-0-l pa2-m db" ] []

-- BUTTONS
backButton : Html msg
backButton =
  H.a
    [ A.class "dib f5 link dim black pa2 ba border-box"
    , A.href "/#shop"
    ]
    [ H.text "< back" ]

consentLink : List (Attribute msg) -> Html msg
consentLink attributes =
  let
    newAttributes =
      List.append
        [ A.id "cookie-consent"
        , A.class "link white underline dim bg-transparent bn pointer"
        , A.title "Consent Preferences"
        ]
        attributes
  in
    H.button newAttributes [ H.text "Cookie Consent"]

-- LAYOUT
section : List (Attribute msg) -> List (Html msg) -> Html msg
section attributes =
  A.class "mt4 mb5" :: attributes
    |> H.section

-- TYPOGRAPHY

h2 : List (Attribute msg) -> List (Html msg) -> Html msg
h2 attributes =
  A.class "f2 fw2 mh2 mv4 bb b--silver" :: attributes
    |> H.h2

h3 : List (Attribute msg) -> List (Html msg) -> Html msg
h3 attributes =
  A.class "f3 fw4 mh2 measure" :: attributes
    |> H.h3

h4 : List (Attribute msg) -> List (Html msg) -> Html msg
h4 attributes =
  A.class "f4 fw4 mh2 measure" :: attributes
    |> H.h4

p : String -> Html msg
p content =
  H.p [ A.class "f4 fw2 mh2 measure" ] [ H.text content ]

ul : List (Html msg) -> Html msg
ul =
  H.ul [ A.class "f4 fw2 mh2 measure" ]

li : String -> Html msg
li content =
  H.li [] [ H.text content ]

-- ICONS
-- Note: This is a bit of a hack that only works with static rendering.
iconInstagram : Html msg
iconInstagram =
  H.text """
<svg fill="currentColor" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" fill-rule="evenodd" clip-rule="evenodd" stroke-linejoin="round" stroke-miterlimit="1.414"><path d="M8 0C5.827 0 5.555.01 4.702.048 3.85.088 3.27.222 2.76.42c-.526.204-.973.478-1.417.923-.445.444-.72.89-.923 1.417-.198.51-.333 1.09-.372 1.942C.008 5.555 0 5.827 0 8s.01 2.445.048 3.298c.04.852.174 1.433.372 1.942.204.526.478.973.923 1.417.444.445.89.72 1.417.923.51.198 1.09.333 1.942.372.853.04 1.125.048 3.298.048s2.445-.01 3.298-.048c.852-.04 1.433-.174 1.942-.372.526-.204.973-.478 1.417-.923.445-.444.72-.89.923-1.417.198-.51.333-1.09.372-1.942.04-.853.048-1.125.048-3.298s-.01-2.445-.048-3.298c-.04-.852-.174-1.433-.372-1.942-.204-.526-.478-.973-.923-1.417-.444-.445-.89-.72-1.417-.923-.51-.198-1.09-.333-1.942-.372C10.445.008 10.173 0 8 0zm0 1.44c2.136 0 2.39.01 3.233.048.78.036 1.203.166 1.485.276.374.145.64.318.92.598.28.28.453.546.598.92.11.282.24.705.276 1.485.038.844.047 1.097.047 3.233s-.01 2.39-.048 3.233c-.036.78-.166 1.203-.276 1.485-.145.374-.318.64-.598.92-.28.28-.546.453-.92.598-.282.11-.705.24-1.485.276-.844.038-1.097.047-3.233.047s-2.39-.01-3.233-.048c-.78-.036-1.203-.166-1.485-.276-.374-.145-.64-.318-.92-.598-.28-.28-.453-.546-.598-.92-.11-.282-.24-.705-.276-1.485C1.45 10.39 1.44 10.136 1.44 8s.01-2.39.048-3.233c.036-.78.166-1.203.276-1.485.145-.374.318-.64.598-.92.28-.28.546-.453.92-.598.282-.11.705-.24 1.485-.276C5.61 1.45 5.864 1.44 8 1.44zm0 2.452c-2.27 0-4.108 1.84-4.108 4.108 0 2.27 1.84 4.108 4.108 4.108 2.27 0 4.108-1.84 4.108-4.108 0-2.27-1.84-4.108-4.108-4.108zm0 6.775c-1.473 0-2.667-1.194-2.667-2.667 0-1.473 1.194-2.667 2.667-2.667 1.473 0 2.667 1.194 2.667 2.667 0 1.473-1.194 2.667-2.667 2.667zm5.23-6.937c0 .53-.43.96-.96.96s-.96-.43-.96-.96.43-.96.96-.96.96.43.96.96z"/></svg>
"""

-- SHOPIFY
buyButton : String -> Html msg
buyButton localId =
  H.div
    [ A.property "data-buy-button" (Encode.string localId)
    , A.class "mv2"
    ] []

soldOut : Html msg
soldOut =
  H.span
    [ A.class "dib f5 br3 pv2 ph3 white bg-black" ]
    [ H.text "Sold Out" ]

cart : Html msg
cart = H.div [ A.id "cart" ] []

-- Currently price comes through with a single decimal place, hence adding a 0
formatGBP : String -> String
formatGBP price =
  "£" ++ price ++ "0"
