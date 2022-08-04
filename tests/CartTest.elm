module CartTest exposing (..)

import CartEvent exposing (CartEvent)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random
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
                    |> Expect.equal [ Cart.LoadCart cartId ]
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
                    |> Expect.equal [ Cart.CreateCart Dict.empty ]
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
            , test "effect is Broadcast CartEvent.CartCreated" <|
                \_ ->
                  Cart.init flagsWithoutCartId
                    |> Tuple.first
                    |> Cart.update (Cart.CartCreated emptyCart)
                    |> Tuple.second
                    |> Expect.equal [ Cart.Broadcast (CartEvent.CartCreated emptyCart.id) ]
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
                    |> Expect.equal [ Cart.CreateCart Dict.empty ]
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
            , test "effect is Broadcast CartEvent.CartCreated" <|
                \_ ->
                  Cart.init flagsWithCartId
                    |> Tuple.first
                    |> Cart.update Cart.CartLoadingFailed
                    |> Tuple.first
                    |> Cart.update (Cart.CartCreated emptyCart)
                    |> Tuple.second
                    |> Expect.equal [ Cart.Broadcast (CartEvent.CartCreated emptyCart.id) ]
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
            , test "effects are empty" <|
                \_ ->
                  Cart.init flagsWithCartId
                    |> Tuple.first
                    |> Cart.update Cart.CartLoadingFailed
                    |> Tuple.first
                    |> Cart.update Cart.CartCreationFailed
                    |> Tuple.second
                    |> Expect.equal []
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
                            [ Cart.CartLinesAdd
                              { cartId = emptyCart.id
                              , productVariantId = "productVariantId"
                              , quantity = 1
                              }
                            ]
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
                            [ Cart.CartLinesAdd
                              { cartId = emptyCart.id
                              , productVariantId = "productVariantId"
                              , quantity = 1
                              }
                            ]
                ]
            , describe "when user triggers checkout"
                [ test "model.cart stays the same" <|
                    \_ ->
                      modelEmptyCartLoaded
                        |> Cart.update Cart.Checkout
                        |> Tuple.first
                        |> Expect.equal modelEmptyCartLoaded
                , test "effects are empty" <|
                    \_ ->
                      modelEmptyCartLoaded
                        |> Cart.update Cart.Checkout
                        |> Tuple.second
                        |> Expect.equal []
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
                            [ Cart.CartLinesUpdate
                              { cartId = cartWithSingleItem.id
                              , lineId = cartLineA.id
                              , quantity = 2
                              }
                            ]
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
                          [ Cart.CartLinesUpdate
                            { cartId = cartWithSingleItem.id
                            , lineId = cartLineA.id
                            , quantity = 2
                            }
                          ]
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
                          [ Cart.CartLinesRemove
                            { cartId = cartWithSingleItem.id
                            , lineId = cartLineA.id
                            }
                          ]
                ]
            , describe "when user triggers checkout"
                [ test "model.cart stays the same" <|
                    \_ ->
                      modelCartWithSingleItemLoaded
                        |> Cart.update Cart.Checkout
                        |> Tuple.first
                        |> Expect.equal modelCartWithSingleItemLoaded
                , test "effects have StartCheckout" <|
                    \_ ->
                      modelCartWithSingleItemLoaded
                        |> Cart.update Cart.Checkout
                        |> Tuple.second
                        |> List.member (Cart.StartCheckout cartWithSingleItem.checkoutUrl)
                        |> Expect.true "Expected list to contain StartCheckout"
                , test "effects have Broadcast CartEvent.CheckoutStarted" <|
                    \_ ->
                      modelCartWithSingleItemLoaded
                        |> Cart.update Cart.Checkout
                        |> Tuple.second
                        |> List.member (Cart.Broadcast CartEvent.CheckoutStarted)
                        |> Expect.true "Expected list to contain Broadcast CartEvent.CheckoutStarted"
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
                            [ Cart.CartLinesUpdate
                              { cartId = cartWithSingleItemMultipleQuantity.id
                              , lineId = cartLineA.id
                              , quantity = 3
                              }
                            ]
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
                          [ Cart.CartLinesUpdate
                            { cartId = cartWithSingleItemMultipleQuantity.id
                            , lineId = cartLineA.id
                            , quantity = 1
                            }
                          ]
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
                , test "effects are Broadcast CartEvent.AddedToCart" <|
                    \_ ->
                      modelCartWithSingleItemUpdating
                        |> Cart.update (Cart.CartUpdated cartWithSingleItemMultipleQuantity)
                        |> Tuple.second
                        |> Expect.equal [ Cart.Broadcast CartEvent.AddedToCart ]
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
                          |> Expect.equal [ Cart.CreateCart linesToCreate ]
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
                , test "effects are empty" <|
                    \_ ->
                      modelCartWithMultipleItemsUpdating
                        |> Cart.update (Cart.CartUpdated cartWithSingleItemMultipleQuantity)
                        |> Tuple.second
                        |> Expect.equal []
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
                          |> Expect.equal [ Cart.CreateCart linesToCreate ]
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
                          |> Expect.equal [ Cart.CreateCart linesToCreate ]
                ]
            ]
        , describe "when CheckoutCompleted"
            [ test "model.cart is Loading" <|
                \_ ->
                  modelCartWithMultipleItemsLoaded
                    |> Cart.update (Cart.CheckoutCompleted)
                    |> Tuple.first
                    |> .cart
                    |> Expect.equal Cart.Loading
            , test "effects have CreateCart with no items" <|
                \_ ->
                  modelCartWithMultipleItemsLoaded
                    |> Cart.update (Cart.CheckoutCompleted)
                    |> Tuple.second
                    |> List.member (Cart.CreateCart Dict.empty)
                    |> Expect.true "Expected list to contain Cart.CreateCart"
            , test "effects have BroadCast CartEvent.CheckoutCompleted" <|
                \_ ->
                  modelCartWithMultipleItemsLoaded
                    |> Cart.update (Cart.CheckoutCompleted)
                    |> Tuple.second
                    |> List.member (Cart.Broadcast CartEvent.CheckoutCompleted)
                    |> Expect.true "Expected list to contain Broadcast CartEvent.CheckoutCompleted"
            ]
        ]


    -- TODO: implement returning diff of items with the event
    , describe "calculateCartChangeEvent"
        [ describe "when RemoteCart has no cart and new cart has items"
            [ fuzz2
                remoteCartWithNoCartFuzzer
                cartWithItemsFuzzer
                "effect is Broadcast CartEvent.AddedToCart" <|
                  \a b ->
                    Cart.calculateCartChangeEvent a b
                      |> Expect.equal [ Cart.Broadcast CartEvent.AddedToCart ]
            ]
        , describe "when RemoteCart has no cart and new cart no items"
            [ fuzz2
                remoteCartWithNoCartFuzzer
                cartWithNoItemsFuzzer
                "effect is empty list" <|
                  \a b ->
                    Cart.calculateCartChangeEvent a b
                      |> Expect.equal []
            ]
        , describe "when RemoteCart has existing cart and new cart has same items"
            [ fuzz
                remoteCartWithUpdatingCartAndCartWithSameItems
                "effect is empty list" <|
                  \(a, b) ->
                    Cart.calculateCartChangeEvent a b
                      |> Expect.equal []
            ]
        , describe "when RemoteCart has existing cart and new cart has items added"
            [ fuzz
                remoteCartWithUpdatingCartAndCartWithAddedItems
                "effect is Broadcast CartEvent.AddedToCart" <|
                  \(a, b) ->
                    Cart.calculateCartChangeEvent a b
                      |> Expect.equal [ Cart.Broadcast CartEvent.AddedToCart ]
            ]
        , describe "when RemoteCart has existing cart and new cart has items removed"
            [ fuzz
                remoteCartWithUpdatingCartAndCartWithRemovedItems
                "effect is Broadcast CartEvent.RemovedFromCart" <|
                  \(a, b) ->
                    Cart.calculateCartChangeEvent a b
                      |> Expect.equal [ Cart.Broadcast CartEvent.RemovedFromCart ]
            ]
        ]
    ]

-- Fuzzers

nonEmptyListFuzzer : Fuzzer a -> Fuzzer (List a)
nonEmptyListFuzzer fuzzer =
  Fuzz.map2 (::) fuzzer (Fuzz.list fuzzer)

cartFuzzer : Fuzzer Api.Cart
cartFuzzer =
  Fuzz.map4 Api.Cart
    Fuzz.string
    Fuzz.string
    (Fuzz.list cartLineFuzzer)
    moneyFuzzer

cartWithNoItemsFuzzer : Fuzzer Api.Cart
cartWithNoItemsFuzzer =
  Fuzz.map4 Api.Cart
    Fuzz.string
    Fuzz.string
    (Fuzz.constant [])
    (Fuzz.constant { amount = "0.0", currencyCode = "GBP" })

cartWithItemsFuzzer : Fuzzer Api.Cart
cartWithItemsFuzzer =
  Fuzz.map4 Api.Cart
    Fuzz.string
    Fuzz.string
    (nonEmptyListFuzzer cartLineFuzzer)
    moneyFuzzer

cartLineFuzzer : Fuzzer Api.CartLine
cartLineFuzzer =
  Fuzz.map4 Api.CartLine
    Fuzz.string
    productVariantFuzzer
    (Fuzz.intRange 1 Random.maxInt)
    moneyFuzzer

changeFuzzer : Fuzzer Cart.Change
changeFuzzer =
  Fuzz.map2 Tuple.pair
    Fuzz.string
    (Fuzz.intRange 1 Random.maxInt)

imageFuzzer : Fuzzer Api.Image
imageFuzzer =
  Fuzz.map Api.Image
    Fuzz.string

moneyFuzzer : Fuzzer Api.Money
moneyFuzzer =
  Fuzz.map2 Api.Money
    (Fuzz.float |> Fuzz.map String.fromFloat)
    (Fuzz.constant "GBP")

productFuzzer : Fuzzer Api.Product
productFuzzer =
  Fuzz.map2 Api.Product
    Fuzz.string
    Fuzz.string

productVariantFuzzer : Fuzzer Api.ProductVariant
productVariantFuzzer =
  Fuzz.map4 Api.ProductVariant
    Fuzz.string
    (Fuzz.maybe imageFuzzer)
    productFuzzer
    (Fuzz.intRange 0 Random.maxInt)

remoteCartWithNoCartFuzzer : Fuzzer Cart.RemoteCart
remoteCartWithNoCartFuzzer =
  Fuzz.oneOf
    [ Fuzz.constant (Cart.Loading)
    , Fuzz.constant (Cart.CreationFailed)
    ]

remoteCartMapperFuzzer : Fuzzer (Api.Cart -> Cart.Change -> Cart.RemoteCart)
remoteCartMapperFuzzer =
  Fuzz.oneOf
    [ Fuzz.constant ((\ cart change -> Cart.Loaded cart ))
    , Fuzz.constant (Cart.Updating)
    , Fuzz.constant (Cart.Recreating)
    ]

remoteCartWithUpdatingCartAndCartWithSameItems : Fuzzer (Cart.RemoteCart, Api.Cart)
remoteCartWithUpdatingCartAndCartWithSameItems =
  Fuzz.map3 (\ remoteCartMapper cart change ->
      (remoteCartMapper cart change, cart)
    )
  remoteCartMapperFuzzer
  cartFuzzer
  changeFuzzer

remoteCartWithUpdatingCartAndCartWithAddedItems : Fuzzer (Cart.RemoteCart, Api.Cart)
remoteCartWithUpdatingCartAndCartWithAddedItems =
  Fuzz.map4 (\ remoteCartMapper cart change quantity ->
      let
        updatedLines =
          case cart.lines of
            [] -> []
            (x :: xs) ->
              { x | quantity = x.quantity + quantity } :: xs
        updatedCart = { cart | lines = updatedLines }
      in
        (remoteCartMapper cart change, updatedCart)
    )
  remoteCartMapperFuzzer
  cartWithItemsFuzzer
  changeFuzzer
  (Fuzz.intRange 1 Random.maxInt)


remoteCartWithUpdatingCartAndCartWithRemovedItems : Fuzzer (Cart.RemoteCart, Api.Cart)
remoteCartWithUpdatingCartAndCartWithRemovedItems =
  Fuzz.map4 (\ remoteCartMapper cart change quantity ->
      let
        updatedLines =
          case cart.lines of
            [] -> []
            (x :: xs) ->
              { x | quantity = max 0 (x.quantity - quantity) } :: xs
        updatedCart = { cart | lines = updatedLines }
      in
        (remoteCartMapper cart change, updatedCart)
    )
  remoteCartMapperFuzzer
  cartWithItemsFuzzer
  changeFuzzer
  (Fuzz.intRange 1 Random.maxInt)
