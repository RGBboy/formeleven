module CartEvent exposing (..)

import Decimal
import Dict exposing (Dict)
import Json.Encode as Encode
import Money exposing (Money)

type CartEvent
  = CartCreated String
  | AddedToCart CartChange
  | RemovedFromCart CartChange
  | CheckoutStarted CartState
  | CheckoutCompleted String CartState

-- TODO: Figure out how best to surface IDs
-- They should all follow the same convention to ensure that
-- the various services we use can associate product correctly
type alias CartItem =
  { price : Money
  , productId : String
  , productVariantId : String
  , quantity : Int
  }

type alias CartState =
  { subTotal : Money
  , items : Dict String CartItem
  }

type alias CartChange = CartState

-- These are encoded to work with Google's expected data structure
encodeItem : CartItem -> Encode.Value
encodeItem item =
  Encode.object
    [ ( "item_id", Encode.string item.productId )
    , ( "price", Encode.float <| Decimal.toFloat item.price.amount )
    , ( "quantity", Encode.int item.quantity )
    , ( "item_variant", Encode.string item.productVariantId )
    ]

-- These are encoded to work with Google's expected data structure
encodeEventValue : Maybe String -> CartChange -> Encode.Value
encodeEventValue maybeTransactionId change =
  let
    encodedTransactionId =
      maybeTransactionId
        |> Maybe.map (\ value -> ( "transaction_id", Encode.string value ))
        |> Maybe.map List.singleton
        |> Maybe.withDefault []
    items =
      change.items
        |> Dict.toList
        |> List.map Tuple.second
  in
    List.append
      [ ( "currency", Encode.string change.subTotal.currencyCode )
      , ( "value", Encode.float <| Decimal.toFloat change.subTotal.amount )
      , ( "items", Encode.list encodeItem items )
      ]
      encodedTransactionId
      |> Encode.object

encode : CartEvent -> Encode.Value
encode event =
  case event of
    CartCreated cartId ->
      Encode.object
        [ ( "type", Encode.string "CartCreated" )
        , ( "value", Encode.string cartId )
        ]

    AddedToCart change ->
      Encode.object
        [ ( "type", Encode.string "AddedToCart" )
        , ( "value", encodeEventValue Nothing change )
        ]

    RemovedFromCart change ->
      Encode.object
        [ ( "type", Encode.string "RemovedFromCart" )
        , ( "value", encodeEventValue Nothing change )
        ]

    CheckoutStarted cartState ->
      Encode.object
        [ ( "type", Encode.string "CheckoutStarted" )
        , ( "value", encodeEventValue Nothing cartState )
        ]

    CheckoutCompleted transactionId cartState ->
      Encode.object
        [ ( "type", Encode.string "CheckoutCompleted" )
        , ( "value", encodeEventValue (Just transactionId) cartState )
        ]
