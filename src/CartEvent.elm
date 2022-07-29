module CartEvent exposing (..)

import Json.Encode as Encode

type CartEvent
  = CartCreated String
  | AddedToCart
  | CheckoutStarted
  | CheckoutCompleted

encode : CartEvent -> Encode.Value
encode event =
  case event of
    CartCreated cartId ->
      Encode.object
        [ ( "type", Encode.string "CartCreated" )
        , ( "value", Encode.string cartId )
        ]

    AddedToCart ->
      Encode.object
        [ ( "type", Encode.string "AddedToCart" )
        ]

    CheckoutStarted ->
      Encode.object
        [ ( "type", Encode.string "CheckoutStarted" )
        ]

    CheckoutCompleted ->
      Encode.object
        [ ( "type", Encode.string "CheckoutCompleted" )
        ]
