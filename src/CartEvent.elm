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

localProductIdFromGlobalProductId : String -> String
localProductIdFromGlobalProductId globalId =
  String.replace "gid://shopify/Product/" "" globalId

localProductVariantIdFromGlobalProductVariantId : String -> String
localProductVariantIdFromGlobalProductVariantId globalId =
  String.replace "gid://shopify/ProductVariant/" "" globalId

-- These are encoded to work with Google Analytics expected data structure
encodeItem : CartItem -> Encode.Value
encodeItem item =
  Encode.object
    [ ( "item_id"
      , item.productId
          |> localProductIdFromGlobalProductId
          |> Encode.string
      )
    , ( "price"
      , item.price.amount
          |> Decimal.toFloat
          |> Encode.float
      )
    , ( "quantity", item.quantity |> Encode.int )
    , ( "item_variant"
      , item.productVariantId
          |> localProductVariantIdFromGlobalProductVariantId
          |> Encode.string
      )
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
      [ ( "currency", change.subTotal.currencyCode |> Encode.string )
      , ( "value"
        , change.subTotal.amount
            |> Decimal.toFloat
            |> Encode.float
        )
      , ( "items", items |> Encode.list encodeItem )
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
