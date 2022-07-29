module CartTest exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (..)

import Cart
import Api

cartId : String
cartId = "cartId"

flagsWithCartId : Cart.Flags
flagsWithCartId =
  { endpoint = "endpoint"
  , token = "token"
  , cartId = Just cartId
  }

flagsWithoutCartId : Cart.Flags
flagsWithoutCartId =
  { endpoint = "endpoint"
  , token = "token"
  , cartId = Nothing
  }

apiConfig : Api.Config
apiConfig =
  { url = "url"
  , token = "token"
  }

cartLineA : Api.CartLine
cartLineA =
  { id = "cartLineAId"
  , productVariant =
      { id = "productVariantAId"
      , image = Nothing
      , product =
          { id = "productAId"
          , title = "Product A Title"
          }
      , quantityAvailable = 5
      }
  , quantity = 1
  , subTotal = zeroCost
  }

multipleQuantityLine : Api.CartLine
multipleQuantityLine = { cartLineA | quantity = 2 }

cartLineB : Api.CartLine
cartLineB =
  { id = "cartLineBId"
  , productVariant =
      { id = "productVariantBId"
      , image = Nothing
      , product =
          { id = "productBId"
          , title = "Product B Title"
          }
      , quantityAvailable = 5
      }
  , quantity = 1
  , subTotal = zeroCost
  }

zeroCost : Api.Money
zeroCost =
  { amount = "0.0"
  , currencyCode = "GBP"
  }

emptyCart : Api.Cart
emptyCart =
  { id = "cartId"
  , checkoutUrl = "checkoutUrl"
  , lines = []
  , subTotal = zeroCost
  }

cartWithSingleItem : Api.Cart
cartWithSingleItem =
  { id = "cartId"
  , checkoutUrl = "checkoutUrl"
  , lines = [ cartLineA ]
  , subTotal = zeroCost
  }

cartWithSingleItemMultipleQuantity : Api.Cart
cartWithSingleItemMultipleQuantity =
  { cartWithSingleItem | lines = [ multipleQuantityLine ] }

cartWithMultipleItems : Api.Cart
cartWithMultipleItems =
  { cartWithSingleItem | lines = [ cartLineA, cartLineB ] }

modelWithLoadedCart : Api.Cart -> Cart.Model
modelWithLoadedCart cart =
  Cart.init flagsWithCartId
    |> Tuple.first
    |> Cart.update (Cart.CartLoaded cart)
    |> Tuple.first

modelEmptyCartLoaded : Cart.Model
modelEmptyCartLoaded =
  modelWithLoadedCart emptyCart

modelCartWithSingleItemLoaded : Cart.Model
modelCartWithSingleItemLoaded =
  modelWithLoadedCart cartWithSingleItem

modelCartWithSingleItemMultipleQuantityLoaded : Cart.Model
modelCartWithSingleItemMultipleQuantityLoaded =
  modelWithLoadedCart cartWithSingleItemMultipleQuantity

modelCartWithMultipleItemsLoaded : Cart.Model
modelCartWithMultipleItemsLoaded =
  modelWithLoadedCart cartWithMultipleItems

modelCartWithSingleItemUpdating : Cart.Model
modelCartWithSingleItemUpdating =
  modelCartWithSingleItemLoaded
    |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 2)
    |> Tuple.first

modelCartWithMultipleItemsUpdating : Cart.Model
modelCartWithMultipleItemsUpdating =
  modelCartWithMultipleItemsLoaded
    |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 2)
    |> Tuple.first

suite : Test
suite =
  describe "Flow"
    [ describe "init"
        [ describe "when passed flags with cartId"
            [ test "model.cart is Loading" <|
                \_ ->
                  Cart.init flagsWithCartId
                    |> Tuple.first
                    |> .cart
                    |> Expect.equal Cart.Loading
            , test "effect is LoadCart" <|
                \_ ->
                  Cart.init flagsWithCartId
                    |> Tuple.second
                    |> Expect.equal (Cart.LoadCart cartId)
            ]
        , describe "when passed flags without cartId"
            [ test "model.cart is Loading" <|
                \_ ->
                  Cart.init flagsWithoutCartId
                    |> Tuple.first
                    |> .cart
                    |> Expect.equal Cart.Loading
            , test "effect is CreateCart with no lines" <|
                \_ ->
                  Cart.init flagsWithoutCartId
                    |> Tuple.second
                    |> Expect.equal (Cart.CreateCart Dict.empty)
            ]
        ]
    , describe "update"
        [ describe "when create cart succeeds"
            [ test "model.cart is Loaded with cart" <|
                \_ ->
                  Cart.init flagsWithoutCartId
                    |> Tuple.first
                    |> Cart.update (Cart.CartCreated emptyCart)
                    |> Tuple.first
                    |> .cart
                    |> Expect.equal (Cart.Loaded emptyCart)
            , test "effect is BroadcastCartCreated" <|
                \_ ->
                  Cart.init flagsWithoutCartId
                    |> Tuple.first
                    |> Cart.update (Cart.CartCreated emptyCart)
                    |> Tuple.second
                    |> Expect.equal (Cart.BroadcastCartCreated emptyCart.id)
            ]
        , describe "when create cart fails"
            [ test "model.cart is CreationFailed" <|
                \_ ->
                  Cart.init flagsWithoutCartId
                    |> Tuple.first
                    |> Cart.update Cart.CartCreationFailed
                    |> Tuple.first
                    |> .cart
                    |> Expect.equal Cart.CreationFailed
            ]
        , describe "when load cart succeeds"
            [ test "model.cart is Loaded with cart" <|
                \_ ->
                  Cart.init flagsWithCartId
                    |> Tuple.first
                    |> Cart.update (Cart.CartLoaded emptyCart)
                    |> Tuple.first
                    |> .cart
                    |> Expect.equal (Cart.Loaded emptyCart)
            ]
        , describe "when load cart fails"
            [ test "model.cart is Loading" <|
                \_ ->
                  Cart.init flagsWithCartId
                    |> Tuple.first
                    |> Cart.update Cart.CartLoadingFailed
                    |> Tuple.first
                    |> .cart
                    |> Expect.equal Cart.Loading
            , test "effect is CreateCart with no lines" <|
                \_ ->
                  Cart.init flagsWithCartId
                    |> Tuple.first
                    |> Cart.update Cart.CartLoadingFailed
                    |> Tuple.second
                    |> Expect.equal (Cart.CreateCart Dict.empty)
            ]
        , describe "and create cart succeeds"
            [ test "model.cart is empty cart" <|
                \_ ->
                  Cart.init flagsWithCartId
                    |> Tuple.first
                    |> Cart.update Cart.CartLoadingFailed
                    |> Tuple.first
                    |> Cart.update (Cart.CartCreated emptyCart)
                    |> Tuple.first
                    |> .cart
                    |> Expect.equal (Cart.Loaded emptyCart)
            , test "effect is BroadcastCartCreated" <|
                \_ ->
                  Cart.init flagsWithCartId
                    |> Tuple.first
                    |> Cart.update Cart.CartLoadingFailed
                    |> Tuple.first
                    |> Cart.update (Cart.CartCreated emptyCart)
                    |> Tuple.second
                    |> Expect.equal (Cart.BroadcastCartCreated emptyCart.id)
            ]
        , describe "and create cart fails"
            [ test "model.cart is CreationFailed" <|
                \_ ->
                  Cart.init flagsWithCartId
                    |> Tuple.first
                    |> Cart.update Cart.CartLoadingFailed
                    |> Tuple.first
                    |> Cart.update Cart.CartCreationFailed
                    |> Tuple.first
                    |> .cart
                    |> Expect.equal Cart.CreationFailed
            , test "effect is Noop" <|
                \_ ->
                  Cart.init flagsWithCartId
                    |> Tuple.first
                    |> Cart.update Cart.CartLoadingFailed
                    |> Tuple.first
                    |> Cart.update Cart.CartCreationFailed
                    |> Tuple.second
                    |> Expect.equal Cart.Noop
            ]
        , describe "when empty cart Loaded"
            [ describe "when user AddToCart"
                [ test "model.cart is Updating with correct quantity" <|
                    \_ ->
                      modelEmptyCartLoaded
                        |> Cart.update (Cart.AddToCart "productVariantId")
                        |> Tuple.first
                        |> .cart
                        |> Expect.equal (Cart.Updating emptyCart ("productVariantId", 1))
                , test "effect is CartLinesAdd with correct quantity" <|
                    \_ ->
                      modelEmptyCartLoaded
                        |> Cart.update (Cart.AddToCart "productVariantId")
                        |> Tuple.second
                        |> Expect.equal
                            ( Cart.CartLinesAdd
                              { cartId = emptyCart.id
                              , productVariantId = "productVariantId"
                              , quantity = 1
                              }
                            )
                ]
            , describe "when user UpdateCart"
                [ test "model.cart is Updating" <|
                    \_ ->
                      modelEmptyCartLoaded
                        |> Cart.update (Cart.UpdateCart "productVariantId" 1)
                        |> Tuple.first
                        |> .cart
                        |> Expect.equal (Cart.Updating emptyCart ("productVariantId", 1))
                , test "effect is CartLinesAdd" <|
                    \_ ->
                      modelEmptyCartLoaded
                        |> Cart.update (Cart.UpdateCart "productVariantId" 1)
                        |> Tuple.second
                        |> Expect.equal
                            ( Cart.CartLinesAdd
                              { cartId = emptyCart.id
                              , productVariantId = "productVariantId"
                              , quantity = 1
                              }
                            )
                ]
            , describe "when user triggers checkout"
                [ test "model.cart stays the same" <|
                    \_ ->
                      modelEmptyCartLoaded
                        |> Cart.update Cart.Checkout
                        |> Tuple.first
                        |> Expect.equal modelEmptyCartLoaded
                , test "effect is Noop" <|
                    \_ ->
                      modelEmptyCartLoaded
                        |> Cart.update Cart.Checkout
                        |> Tuple.second
                        |> Expect.equal Cart.Noop
                ]
            ]
        , describe "when cart with single item is Loaded"
            [ describe "when user AddToCart"
                [ test "model.cart is Updating with correct quantity" <|
                    \_ ->
                      modelCartWithSingleItemLoaded
                        |> Cart.update (Cart.AddToCart cartLineA.productVariant.id)
                        |> Tuple.first
                        |> .cart
                        |> Expect.equal (Cart.Updating cartWithSingleItem (cartLineA.productVariant.id, 2))
                , test "effect is CartLinesAdd with correct quantity" <|
                    \_ ->
                      modelCartWithSingleItemLoaded
                        |> Cart.update (Cart.AddToCart cartLineA.productVariant.id)
                        |> Tuple.second
                        |> Expect.equal
                            ( Cart.CartLinesUpdate
                              { cartId = cartWithSingleItem.id
                              , lineId = cartLineA.id
                              , quantity = 2
                              }
                            )
                ]
            , describe "when user UpdateCart"
                [ test "model.cart is Updating" <|
                    \_ ->
                      modelCartWithSingleItemLoaded
                        |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 2)
                        |> Tuple.first
                        |> .cart
                        |> Expect.equal (Cart.Updating cartWithSingleItem (cartLineA.productVariant.id, 2))
                , test "effect is CartLinesUpdate" <|
                    \_ ->
                      modelCartWithSingleItemLoaded
                        |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 2)
                        |> Tuple.second
                        |> Expect.equal
                          ( Cart.CartLinesUpdate
                            { cartId = cartWithSingleItem.id
                            , lineId = cartLineA.id
                            , quantity = 2
                            }
                          )
                ]
            , describe "when user removes all of an item"
                [ test "model.cart is Updating" <|
                    \_ ->
                      modelCartWithSingleItemLoaded
                        |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 0)
                        |> Tuple.first
                        |> .cart
                        |> Expect.equal (Cart.Updating cartWithSingleItem (cartLineA.productVariant.id, 0))
                , test "effect is CartLinesRemove" <|
                    \_ ->
                      modelCartWithSingleItemLoaded
                        |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 0)
                        |> Tuple.second
                        |> Expect.equal
                          ( Cart.CartLinesRemove
                            { cartId = cartWithSingleItem.id
                            , lineId = cartLineA.id
                            }
                          )
                ]
            , describe "when user triggers checkout"
                [ test "model.cart stays the same" <|
                    \_ ->
                      modelCartWithSingleItemLoaded
                        |> Cart.update Cart.Checkout
                        |> Tuple.first
                        |> Expect.equal modelCartWithSingleItemLoaded
                , test "effect is Noop" <|
                    \_ ->
                      modelCartWithSingleItemLoaded
                        |> Cart.update Cart.Checkout
                        |> Tuple.second
                        |> Expect.equal (Cart.StartCheckout cartWithSingleItem.checkoutUrl)
                ]
            ]
        , describe "when cart with single item and multiple quantity is Loaded"
            [ describe "when user AddToCart"
                [ test "model.cart is Updating with correct quantity" <|
                    \_ ->
                      modelCartWithSingleItemMultipleQuantityLoaded
                        |> Cart.update (Cart.AddToCart cartLineA.productVariant.id)
                        |> Tuple.first
                        |> .cart
                        |> Expect.equal (Cart.Updating cartWithSingleItemMultipleQuantity (cartLineA.productVariant.id, 3))
                , test "effect is CartLinesAdd with correct quantity" <|
                    \_ ->
                      modelCartWithSingleItemMultipleQuantityLoaded
                        |> Cart.update (Cart.AddToCart cartLineA.productVariant.id)
                        |> Tuple.second
                        |> Expect.equal
                            ( Cart.CartLinesUpdate
                              { cartId = cartWithSingleItemMultipleQuantity.id
                              , lineId = cartLineA.id
                              , quantity = 3
                              }
                            )
                ]
            , describe "when user removes item"
                [ test "model.cart is Updating" <|
                    \_ ->
                      modelCartWithSingleItemMultipleQuantityLoaded
                        |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 1)
                        |> Tuple.first
                        |> .cart
                        |> Expect.equal (Cart.Updating cartWithSingleItemMultipleQuantity (cartLineA.productVariant.id, 1))
                , test "effect is CartLinesUpdate" <|
                    \_ ->
                      modelCartWithSingleItemMultipleQuantityLoaded
                        |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 1)
                        |> Tuple.second
                        |> Expect.equal
                          ( Cart.CartLinesUpdate
                            { cartId = cartWithSingleItemMultipleQuantity.id
                            , lineId = cartLineA.id
                            , quantity = 1
                            }
                          )
                ]
            ]
        , describe "when cart with single item is Updating"
            [ describe "and message is CartUpdated"
                [ test "model.cart is updated cart" <|
                    \_ ->
                      modelCartWithSingleItemUpdating
                        |> Cart.update (Cart.CartUpdated cartWithSingleItemMultipleQuantity)
                        |> Tuple.first
                        |> .cart
                        |> Expect.equal (Cart.Loaded cartWithSingleItemMultipleQuantity)
                , test "effect is Noop" <|
                    \_ ->
                      modelCartWithSingleItemUpdating
                        |> Cart.update (Cart.CartUpdated cartWithSingleItemMultipleQuantity)
                        |> Tuple.second
                        |> Expect.equal (Cart.Noop)
                ]
            , describe "and message is CartUpdateFailed"
                -- handles the case for expired cart
                [ test "model.cart is Recreating" <|
                    \_ ->
                      modelCartWithSingleItemUpdating
                        |> Cart.update (Cart.CartUpdateFailed)
                        |> Tuple.first
                        |> .cart
                        |> Expect.equal (Cart.Recreating cartWithSingleItem (cartLineA.productVariant.id, 2))
                , test "effect is CreateCart" <|
                    \_ ->
                      let
                        linesToCreate =
                          [ (cartLineA.productVariant.id, 2) ]
                            |> Dict.fromList
                      in
                        modelCartWithSingleItemUpdating
                          |> Cart.update (Cart.CartUpdateFailed)
                          |> Tuple.second
                          |> Expect.equal (Cart.CreateCart linesToCreate)
                ]
            ]
        , describe "when cart with multiple items is Updating"
            [ describe "and message is CartUpdated"
                [ test "model.cart is updated cart" <|
                    \_ ->
                      modelCartWithMultipleItemsUpdating
                        |> Cart.update (Cart.CartUpdated cartWithSingleItemMultipleQuantity)
                        |> Tuple.first
                        |> .cart
                        |> Expect.equal (Cart.Loaded cartWithSingleItemMultipleQuantity)
                , test "effect is Noop" <|
                    \_ ->
                      modelCartWithMultipleItemsUpdating
                        |> Cart.update (Cart.CartUpdated cartWithSingleItemMultipleQuantity)
                        |> Tuple.second
                        |> Expect.equal (Cart.Noop)
                ]
            , describe "and msg is CartUpdateFailed"
                [ test "model.cart is recreating cart" <|
                    \_ ->
                      modelCartWithMultipleItemsUpdating
                        |> Cart.update (Cart.CartUpdateFailed)
                        |> Tuple.first
                        |> .cart
                        |> Expect.equal (Cart.Recreating cartWithMultipleItems (cartLineA.productVariant.id, 2))
                , test "effect is CreateCart with all lines" <|
                    \_ ->
                      let
                        linesToCreate =
                          [ (cartLineA.productVariant.id, 2)
                          , (cartLineB.productVariant.id, 1)
                          ]
                            |> Dict.fromList
                      in
                        modelCartWithMultipleItemsUpdating
                          |> Cart.update (Cart.CartUpdateFailed)
                          |> Tuple.second
                          |> Expect.equal (Cart.CreateCart linesToCreate)
                , test "effect is CreateCart without removed lines" <|
                    \_ ->
                      let
                        linesToCreate =
                          [ (cartLineB.productVariant.id, 1) ]
                            |> Dict.fromList
                        model =
                          modelCartWithMultipleItemsLoaded
                            |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 0)
                            |> Tuple.first
                      in
                        model
                          |> Cart.update (Cart.CartUpdateFailed)
                          |> Tuple.second
                          |> Expect.equal (Cart.CreateCart linesToCreate)
                ]
            ]
        ]
    ]
