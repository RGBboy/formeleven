module CartEvent exposing (..)

import Decimal
import Dict exposing (Dict)
import Json.Encode as Encode
import Money exposing (Money)

type CartEvent
  = CartCreated String
  | AddedToCart CartChange
  | RemovedFromCart CartChange
  | CheckoutStarted
  | CheckoutCompleted

-- TODO: Figure out how best to surface IDs
-- They should all follow the same convention to ensure that
-- the various services we use can associate product correctly
type alias CartChangeItem =
  { price : Money
  , productId : String
  , productVariantId : String
  , quantity : Int
  }

type alias CartChange =
  { subTotal : Money
  , items : Dict String CartChangeItem
  }

-- These are encoded to work with Google's expected data structure
encodeCartChangeItem : CartChangeItem -> Encode.Value
encodeCartChangeItem item =
  Encode.object
    [ ( "item_id", Encode.string item.productId )
    , ( "price", Encode.float <| Decimal.toFloat item.price.amount )
    , ( "quantity", Encode.int item.quantity )
    , ( "item_variant", Encode.string item.productVariantId )
    ]

-- These are encoded to work with Google's expected data structure
encodeCartChange : CartChange -> Encode.Value
encodeCartChange change =
  let
    items =
      change.items
        |> Dict.toList
        |> List.map Tuple.second
  in
    Encode.object
      [ ( "currency", Encode.string change.subTotal.currencyCode )
      , ( "value", Encode.float <| Decimal.toFloat change.subTotal.amount )
      , ( "items", Encode.list encodeCartChangeItem items )
      ]

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
        , ( "value", encodeCartChange change )
        ]

    RemovedFromCart change ->
      Encode.object
        [ ( "type", Encode.string "RemovedFromCart" )
        , ( "value", encodeCartChange change )
        ]

    CheckoutStarted ->
      Encode.object
        [ ( "type", Encode.string "CheckoutStarted" )
        ]

    CheckoutCompleted ->
      Encode.object
        [ ( "type", Encode.string "CheckoutCompleted" )
        ]
