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

multipleQuantityLineA : Api.CartLine
multipleQuantityLineA = { cartLineA | quantity = 2 }

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

cartWithSingleItemUpdated : Api.Cart
cartWithSingleItemUpdated =
  { cartWithSingleItem | lines = [ multipleQuantityLineA ] }

cartWithSingleItemMultipleQuantity : Api.Cart
cartWithSingleItemMultipleQuantity =
  { cartWithSingleItem | lines = [ multipleQuantityLineA ] }

cartWithMultipleItems : Api.Cart
cartWithMultipleItems =
  { cartWithSingleItem | lines = [ cartLineA, cartLineB ] }

cartWithMultipleItemsUpdated : Api.Cart
cartWithMultipleItemsUpdated =
  { cartWithSingleItem | lines = [ multipleQuantityLineA, cartLineB ] }

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
  describe "Cart"
    [ describe "init"
        [ test """
When passed flags with cartId
Then model.cart is Loading
And effect is LoadCart
""" <|
            \_ ->
              let
                result = Cart.init flagsWithCartId
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal Cart.Loading
                  , Tuple.second
                      >> Expect.equal [ Cart.LoadCart cartId ]
                  ]
                  result
        , test """
When passed flags without cartId
Then model.cart is Loading
And effect is CreateCart with no lines
""" <|
            \_ ->
              let
                result = Cart.init flagsWithoutCartId
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal Cart.Loading
                  , Tuple.second
                      >> Expect.equal [ Cart.CreateCart Dict.empty ]
                  ]
                  result
        ]
    , describe "update"
        [ test """
Given model is init without cartId
When CartCreated
Then model.cart is Loaded with cart
And effect is Broadcast CartEvent.CartCreated
""" <|
            \_ ->
              let
                result = Cart.init flagsWithoutCartId
                  |> Tuple.first
                  |> Cart.update (Cart.CartCreated emptyCart)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Loaded emptyCart)
                  , Tuple.second
                      >> Expect.equal [ Cart.Broadcast (CartEvent.CartCreated emptyCart.id) ]
                  ]
                  result
        , test """
Given model is init without cartId
When CartCreationFailed
Then model.cart is CreationFailed
And effect is nothing
""" <|
            \_ ->
              let
                result = Cart.init flagsWithoutCartId
                  |> Tuple.first
                  |> Cart.update Cart.CartCreationFailed
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal Cart.CreationFailed
                  , Tuple.second
                      >> Expect.equal []
                  ]
                  result
        , test """
Given model is init with cartId
When CartLoaded
Then model.cart is Loaded with cart
And effect is nothing
""" <|
            \_ ->
              let
                result = Cart.init flagsWithCartId
                  |> Tuple.first
                  |> Cart.update (Cart.CartLoaded emptyCart)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Loaded emptyCart)
                  , Tuple.second
                      >> Expect.equal []
                  ]
                  result
        , test """
Given model is init with cartId
When CartLoadingFailed
Then model.cart is Loading
And effect is CreateCart with no lines
""" <|
            \_ ->
              let
                result = Cart.init flagsWithCartId
                  |> Tuple.first
                  |> Cart.update Cart.CartLoadingFailed
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal Cart.Loading
                  , Tuple.second
                      >> Expect.equal [ Cart.CreateCart Dict.empty ]
                  ]
                  result
        , test """
Given model is init with cartId
When CartLoadingFailed
And CartCreated
Then model.cart is Loaded with empty cart
And effect is Broadcast CartEvent.CartCreated
""" <|
            \_ ->
              let
                result = Cart.init flagsWithCartId
                  |> Tuple.first
                  |> Cart.update Cart.CartLoadingFailed
                  |> Tuple.first
                  |> Cart.update (Cart.CartCreated emptyCart)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Loaded emptyCart)
                  , Tuple.second
                      >> Expect.equal [ Cart.Broadcast (CartEvent.CartCreated emptyCart.id) ]
                  ]
                  result
        , test """
Given model is init with cartId
When CartLoadingFailed
And CartCreationFailed
Then model.cart is CreationFailed
And effect is nothing
""" <|
            \_ ->
              let
                result = Cart.init flagsWithCartId
                  |> Tuple.first
                  |> Cart.update Cart.CartLoadingFailed
                  |> Tuple.first
                  |> Cart.update Cart.CartCreationFailed
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal Cart.CreationFailed
                  , Tuple.second
                      >> Expect.equal []
                  ]
                  result
        , test """
Given model is init with cartId
When CartLoadingFailed
And CartCreationFailed
And user Retry
Then model.cart is Loading
And effect is CreateCart with no lines
""" <|
            \_ ->
              let
                result = Cart.init flagsWithCartId
                  |> Tuple.first
                  |> Cart.update Cart.CartLoadingFailed
                  |> Tuple.first
                  |> Cart.update Cart.CartCreationFailed
                  |> Tuple.first
                  |> Cart.update Cart.Retry
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal Cart.Loading
                  , Tuple.second
                      >> Expect.equal [ Cart.CreateCart Dict.empty ]
                  ]
                  result
        , test """
Given empty cart is Loaded
When user AddToCart
Then model.cart is Updating with correct quantity
And effect is CartLinesAdd with correct quantity
""" <|
            \_ ->
              let
                result = modelEmptyCartLoaded
                  |> Cart.update (Cart.AddToCart "productVariantId")
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Updating emptyCart ("productVariantId", 1))
                  , Tuple.second
                      >> Expect.equal
                        [ Cart.CartLinesAdd
                            { cartId = emptyCart.id
                            , productVariantId = "productVariantId"
                            , quantity = 1
                            }
                        ]
                  ]
                  result
        , test """
Given empty cart is Loaded
When user UpdateCart with item
Then model.cart is Updating
And effect is CartLinesAdd
""" <|
            \_ ->
              let
                result = modelEmptyCartLoaded
                  |> Cart.update (Cart.UpdateCart "productVariantId" 1)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Updating emptyCart ("productVariantId", 1))
                  , Tuple.second
                      >> Expect.equal
                        [ Cart.CartLinesAdd
                            { cartId = emptyCart.id
                            , productVariantId = "productVariantId"
                            , quantity = 1
                            }
                        ]
                  ]
                  result
        , test """
Given empty cart is Loaded
When user triggers checkout
Then model.cart stays the same
And effect is nothing
""" <|
            \_ ->
              let
                result = modelEmptyCartLoaded
                  |> Cart.update Cart.Checkout
              in
                Expect.all
                  [ Tuple.first
                      >> Expect.equal modelEmptyCartLoaded
                  , Tuple.second
                      >> Expect.equal []
                  ]
                  result
        , test """
Given cart with single item is Loaded
When user AddToCart
Then model.cart is Updating with correct quantity
And effect is CartLinesAdd with correct quantity
""" <|
            \_ ->
              let
                result = modelCartWithSingleItemLoaded
                  |> Cart.update (Cart.AddToCart cartLineA.productVariant.id)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Updating cartWithSingleItem (cartLineA.productVariant.id, 2))
                  , Tuple.second
                      >> Expect.equal
                          [ Cart.CartLinesUpdate
                              { cartId = cartWithSingleItem.id
                              , lineId = cartLineA.id
                              , quantity = 2
                              }
                          ]
                  ]
                  result
        , test """
Given cart with single item is Loaded
When user adds more of an an existing item
Then model.cart is Updating with correct quantity
And effect is CartLinesUpdate with correct quantity
""" <|
            \_ ->
              let
                result = modelCartWithSingleItemLoaded
                  |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 2)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Updating cartWithSingleItem (cartLineA.productVariant.id, 2))
                  , Tuple.second
                      >> Expect.equal
                          [ Cart.CartLinesUpdate
                              { cartId = cartWithSingleItem.id
                              , lineId = cartLineA.id
                              , quantity = 2
                              }
                          ]
                  ]
                  result
        , test """
Given cart with single item is Loaded
When user removes all of an item
Then model.cart is Updating with correct quantity
And effect is CartLinesRemove
""" <|
            \_ ->
              let
                result = modelCartWithSingleItemLoaded
                  |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 0)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Updating cartWithSingleItem (cartLineA.productVariant.id, 0))
                  , Tuple.second
                      >> Expect.equal
                          [ Cart.CartLinesRemove
                              { cartId = cartWithSingleItem.id
                              , lineId = cartLineA.id
                              }
                          ]
                  ]
                  result
        , test """
Given cart with single item is Loaded
When user triggers Checkout
Then model.cart stays the same
And effects have StartCheckout
And effects have Broadcast CartEvent.CheckoutStarted
""" <|
            \_ ->
              let
                result = modelCartWithSingleItemLoaded
                  |> Cart.update Cart.Checkout
              in
                Expect.all
                  [ Tuple.first
                      >> Expect.equal modelCartWithSingleItemLoaded
                  , Tuple.second
                      >> List.member (Cart.StartCheckout cartWithSingleItem.checkoutUrl)
                      >> Expect.true "Expected list to contain StartCheckout"
                  , Tuple.second
                      >> List.member (Cart.Broadcast CartEvent.CheckoutStarted)
                      >> Expect.true "Expected list to contain Broadcast CartEvent.CheckoutStarted"
                  ]
                  result
        , test """
Given cart with single item and multiple quantity is Loaded
When user AddToCart
Then model.cart is Updating with correct quantity
And effect is CartLinesAdd with correct quantity
""" <|
            \_ ->
              let
                result = modelCartWithSingleItemMultipleQuantityLoaded
                  |> Cart.update (Cart.AddToCart cartLineA.productVariant.id)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Updating cartWithSingleItemMultipleQuantity (cartLineA.productVariant.id, 3))
                  , Tuple.second
                      >> Expect.equal
                          [ Cart.CartLinesUpdate
                              { cartId = cartWithSingleItemMultipleQuantity.id
                              , lineId = cartLineA.id
                              , quantity = 3
                              }
                          ]
                  ]
                  result
        , test """
Given cart with single item and multiple quantity is Loaded
When user adds an existing item
Then model.cart is Updating with correct quantity
And effect is CartLinesAdd with correct quantity
""" <|
            \_ ->
              let
                result = modelCartWithSingleItemMultipleQuantityLoaded
                  |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 4)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Updating cartWithSingleItemMultipleQuantity (cartLineA.productVariant.id, 4))
                  , Tuple.second
                      >> Expect.equal
                          [ Cart.CartLinesUpdate
                              { cartId = cartWithSingleItemMultipleQuantity.id
                              , lineId = cartLineA.id
                              , quantity = 4
                              }
                          ]
                  ]
                  result
        , test """
Given cart with single item and multiple quantity is Loaded
When user removes some of an item
Then model.cart is Updating with correct quantity
And effect is CartLinesAdd with correct quantity
""" <|
            \_ ->
              let
                result = modelCartWithSingleItemMultipleQuantityLoaded
                  |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 1)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Updating cartWithSingleItemMultipleQuantity (cartLineA.productVariant.id, 1))
                  , Tuple.second
                      >> Expect.equal
                          [ Cart.CartLinesUpdate
                              { cartId = cartWithSingleItemMultipleQuantity.id
                              , lineId = cartLineA.id
                              , quantity = 1
                              }
                          ]
                  ]
                  result
        , test """
Given cart with single item and multiple quantity is Loaded
When user removes all of an item
Then model.cart is Updating with correct quantity
And effect is CartLinesAdd with correct quantity
""" <|
            \_ ->
              let
                result = modelCartWithSingleItemMultipleQuantityLoaded
                  |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 0)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Updating cartWithSingleItemMultipleQuantity (cartLineA.productVariant.id, 0))
                  , Tuple.second
                      >> Expect.equal
                          [ Cart.CartLinesRemove
                              { cartId = cartWithSingleItemMultipleQuantity.id
                              , lineId = cartLineA.id
                              }
                          ]
                  ]
                  result
        , test """
Given cart with single item is Updating
When CartUpdated
Then model.cart is Loaded with updated cart
And effect is Broadcast CartEvent.AddedToCart
""" <|
            \_ ->
              let
                result = modelCartWithSingleItemUpdating
                  |> Cart.update (Cart.CartUpdated cartWithSingleItemUpdated)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Loaded cartWithSingleItemUpdated)
                  , Tuple.second
                      >> Expect.equal [ Cart.Broadcast CartEvent.AddedToCart ]
                  ]
                  result

-- handles the case for expired cart
        , test """
Given cart with single item is Updating
When CartUpdateFailed
Then model.cart is Recreating with updated cart
And effect is CreateCart
""" <|
            \_ ->
              let
                result = modelCartWithSingleItemUpdating
                  |> Cart.update Cart.CartUpdateFailed
                linesToCreate =
                  [ (cartLineA.productVariant.id, 2) ]
                    |> Dict.fromList
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Recreating cartWithSingleItem (cartLineA.productVariant.id, 2))
                  , Tuple.second
                      >> Expect.equal [ Cart.CreateCart linesToCreate ]
                  ]
                  result
        , test """
Given cart with multiple items is Updating
When CartUpdated
Then model.cart is Loaded with updated cart
And effect is Broadcast CartEvent.AddedToCart
""" <|
            \_ ->
              let
                result = modelCartWithMultipleItemsUpdating
                  |> Cart.update (Cart.CartUpdated cartWithMultipleItemsUpdated)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Loaded cartWithMultipleItemsUpdated)
                  , Tuple.second
                      >> Expect.equal [ Cart.Broadcast CartEvent.AddedToCart ]
                  ]
                  result
        , test """
Given cart with multiple items is Updating
When CartUpdateFailed
Then model.cart is Recreating
And effect is CreateCart with updated lines and quantities
""" <|
            \_ ->
              let
                result = modelCartWithMultipleItemsUpdating
                  |> Cart.update Cart.CartUpdateFailed
                linesToCreate =
                  [ (cartLineA.productVariant.id, 2)
                  , (cartLineB.productVariant.id, 1)
                  ]
                    |> Dict.fromList
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Recreating cartWithMultipleItems (cartLineA.productVariant.id, 2))
                  , Tuple.second
                      >> Expect.equal [ Cart.CreateCart linesToCreate ]
                  ]
                  result
        , test """
Given cart with multiple items is Updating
And a line has been removed
When CartUpdateFailed
Then model.cart is Recreating
And effect is CreateCart with updated lines and quantities
""" <|
            \_ ->
              let
                result = modelCartWithMultipleItemsLoaded
                  |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 0)
                  |> Tuple.first
                  |> Cart.update Cart.CartUpdateFailed
                linesToCreate =
                  [ (cartLineB.productVariant.id, 1) ]
                    |> Dict.fromList
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Recreating cartWithMultipleItems (cartLineA.productVariant.id, 0))
                  , Tuple.second
                      >> Expect.equal [ Cart.CreateCart linesToCreate ]
                  ]
                  result

        , test """
Given cart is Updating with additional items
When CartUpdateFailed
And cart is recreated with CartCreated
Then model.cart is Loaded with updated cart
And effects have Broadcast CartEvents.CartCreated
And effects have Broadcast CartEvents.AddedToCart
""" <|
            \_ ->
              let
                updatedLineA = { cartLineA | quantity = 3 }
                updatedCart =
                  { cartWithMultipleItems | lines = [ updatedLineA, cartLineB ] }
                result = modelCartWithMultipleItemsLoaded
                  |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 3)
                  |> Tuple.first
                  |> Cart.update Cart.CartUpdateFailed
                  |> Tuple.first
                  |> Cart.update (Cart.CartCreated updatedCart)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Loaded updatedCart)
                  , Tuple.second
                      >> List.member (Cart.Broadcast (CartEvent.CartCreated updatedCart.id))
                      >> Expect.true "Expected list to contain Broadcast CartEvent.CartCreated"
                  , Tuple.second
                      >> List.member (Cart.Broadcast CartEvent.AddedToCart)
                      >> Expect.true "Expected list to contain Broadcast CartEvent.AddedToCart"
                  ]
                  result
        , test """
Given cart with items is Updating with less of an item
When CartUpdateFailed
And cart is recreated with CartCreated
Then model.cart is Loaded with updated cart
And effects have Broadcast CartEvents.CartCreated
And effects have Broadcast CartEvents.RemovedFromCart
""" <|
            \_ ->
              let
                updatedLineA = { cartLineA | quantity = 1 }
                updatedCart =
                  { cartWithSingleItemMultipleQuantity | lines = [ updatedLineA ] }
                result = modelCartWithSingleItemMultipleQuantityLoaded
                  |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 1)
                  |> Tuple.first
                  |> Cart.update Cart.CartUpdateFailed
                  |> Tuple.first
                  |> Cart.update (Cart.CartCreated updatedCart)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Loaded updatedCart)
                  , Tuple.second
                      >> List.member (Cart.Broadcast (CartEvent.CartCreated updatedCart.id))
                      >> Expect.true "Expected list to contain Broadcast CartEvent.CartCreated"
                  , Tuple.second
                      >> List.member (Cart.Broadcast CartEvent.RemovedFromCart)
                      >> Expect.true "Expected list to contain Broadcast CartEvent.RemovedFromCart"
                  ]
                  result
        , test """
Given cart is Updating with removed all of an item
When CartUpdateFailed
And cart is recreated with CartCreated
Then model.cart is Loaded with updated cart
And effects have Broadcast CartEvents.CartCreated
And effects have Broadcast CartEvents.RemovedFromCart
""" <|
            \_ ->
              let
                updatedCart =
                  { cartWithMultipleItems | lines = [ cartLineB ] }
                result = modelCartWithMultipleItemsLoaded
                  |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 0)
                  |> Tuple.first
                  |> Cart.update Cart.CartUpdateFailed
                  |> Tuple.first
                  |> Cart.update (Cart.CartCreated updatedCart)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Loaded updatedCart)
                  , Tuple.second
                      >> List.member (Cart.Broadcast (CartEvent.CartCreated updatedCart.id))
                      >> Expect.true "Expected list to contain Broadcast CartEvent.CartCreated"
                  , Tuple.second
                      >> List.member (Cart.Broadcast CartEvent.RemovedFromCart)
                      >> Expect.true "Expected list to contain Broadcast CartEvent.RemovedFromCart"
                  ]
                  result
        , test """
Given cart is Updating with additional items
When CartUpdateFailed
And CartCreationFailed
And user Retry
Then model.cart is Recreating
And effect is CreateCart with correct lines and quantities
""" <|
            \_ ->
              let
                linesToCreate =
                  [ (cartLineA.productVariant.id, 3)
                  , (cartLineB.productVariant.id, 1)
                  ]
                    |> Dict.fromList
                result = modelCartWithMultipleItemsLoaded
                  |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 3)
                  |> Tuple.first
                  |> Cart.update Cart.CartUpdateFailed
                  |> Tuple.first
                  |> Cart.update Cart.CartCreationFailed
                  |> Tuple.first
                  |> Cart.update Cart.Retry
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Recreating cartWithMultipleItems (cartLineA.productVariant.id, 3) )
                  , Tuple.second
                      >> Expect.equal [ Cart.CreateCart linesToCreate ]
                  ]
                  result
        , test """
Given cart is Updating with additional items
When CartUpdateFailed
And CartCreationFailed
And user Retry
And cart is recreated with CartCreated
Then model.cart is Loaded with updated cart
And effects have Broadcast CartEvents.CartCreated
And effects have Broadcast CartEvents.AddedToCart
""" <|
            \_ ->
              let
                updatedLineA = { cartLineA | quantity = 3 }
                updatedCart =
                  { cartWithMultipleItems | lines = [ updatedLineA, cartLineB ] }
                result = modelCartWithMultipleItemsLoaded
                  |> Cart.update (Cart.UpdateCart cartLineA.productVariant.id 3)
                  |> Tuple.first
                  |> Cart.update Cart.CartUpdateFailed
                  |> Tuple.first
                  |> Cart.update Cart.CartCreationFailed
                  |> Tuple.first
                  |> Cart.update Cart.Retry
                  |> Tuple.first
                  |> Cart.update (Cart.CartCreated updatedCart)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal (Cart.Loaded updatedCart)
                  , Tuple.second
                      >> List.member (Cart.Broadcast (CartEvent.CartCreated updatedCart.id))
                      >> Expect.true "Expected list to contain Broadcast CartEvent.CartCreated"
                  , Tuple.second
                      >> List.member (Cart.Broadcast CartEvent.AddedToCart)
                      >> Expect.true "Expected list to contain Broadcast CartEvent.AddedToCart"
                  ]
                  result
        , test """
Given cart is Loaded
When CheckoutCompleted
Then model.cart is Loading
And effects have CreateCart with no items
And effects have BroadCast CartEvent.CheckoutCompleted
""" <|
            \_ ->
              let
                result = modelCartWithMultipleItemsLoaded
                  |> Cart.update (Cart.CheckoutCompleted)
              in
                Expect.all
                  [ Tuple.first
                      >> .cart
                      >> Expect.equal Cart.Loading
                  , Tuple.second
                      >> List.member (Cart.CreateCart Dict.empty)
                      >> Expect.true "Expected list to contain Cart.CreateCart"
                  , Tuple.second
                      >> List.member (Cart.Broadcast CartEvent.CheckoutCompleted)
                      >> Expect.true "Expected list to contain Broadcast CartEvent.CheckoutCompleted"
                  ]
                  result
        ]


-- TODO: implement returning diff of items with the event
    , describe "calculateCartChangeEvent"
        [ fuzz2
            remoteCartWithNoCartFuzzer
            cartWithItemsFuzzer
            """
When RemoteCart has no cart
And new cart has items
Then effect is Broadcast CartEvent.AddedToCart
""" <|
            \a b ->
              Cart.calculateCartChangeEvent a b
                |> Expect.equal [ Cart.Broadcast CartEvent.AddedToCart ]
        , fuzz2
            remoteCartWithNoCartFuzzer
            cartWithNoItemsFuzzer
            """
When RemoteCart has no cart
And new cart no items
Then effect is empty list
""" <|
            \a b ->
              Cart.calculateCartChangeEvent a b
                |> Expect.equal []
        , fuzz
            remoteCartWithUpdatingCartAndCartWithSameItems
            """
When RemoteCart has existing cart
And new cart has same items
Then effect is empty list
""" <|
            \(a, b) ->
              Cart.calculateCartChangeEvent a b
                |> Expect.equal []
        , fuzz
            remoteCartWithUpdatingCartAndCartWithAddedItems
            """
When RemoteCart has existing cart
And new cart has items added
Then effect is Broadcast CartEvent.AddedToCart
""" <|
            \(a, b) ->
              Cart.calculateCartChangeEvent a b
                |> Expect.equal [ Cart.Broadcast CartEvent.AddedToCart ]
        , fuzz
            remoteCartWithUpdatingCartAndCartWithRemovedItems
            """
When RemoteCart has existing cart
And new cart has items removed
Then effect is Broadcast CartEvent.RemovedFromCart
""" <|
            \(a, b) ->
              Cart.calculateCartChangeEvent a b
                |> Expect.equal [ Cart.Broadcast CartEvent.RemovedFromCart ]
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
    , Fuzz.constant (Cart.RecreationFailed)
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
