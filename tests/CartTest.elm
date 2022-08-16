module CartTest exposing (..)

import Decimal
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Helpers
import Random
import Test exposing (..)

import Api
import Cart
import CartEvent exposing (CartEvent)
import Money exposing (Money)


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
      , price = Helpers.moneyFromInt 1
      , product =
          { id = "productAId"
          , title = "Product A Title"
          }
      , quantityAvailable = 5
      }
  , quantity = 1
  , subTotal = Helpers.moneyFromInt 1
  }

multipleQuantityLineA : Api.CartLine
multipleQuantityLineA =
  { cartLineA
  | quantity = 2
  , subTotal = Helpers.moneyFromInt 2
  }

cartLineB : Api.CartLine
cartLineB =
  { id = "cartLineBId"
  , productVariant =
      { id = "productVariantBId"
      , image = Nothing
      , price = Helpers.moneyFromInt 10
      , product =
          { id = "productBId"
          , title = "Product B Title"
          }
      , quantityAvailable = 5
      }
  , quantity = 1
  , subTotal = Helpers.moneyFromInt 10
  }

zeroCost : Money
zeroCost = Helpers.moneyFromInt 0

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
  , subTotal = cartLineA.subTotal
  }

cartWithSingleItemUpdated : Api.Cart
cartWithSingleItemUpdated =
  { cartWithSingleItem
  | lines = [ multipleQuantityLineA ]
  , subTotal = multipleQuantityLineA.subTotal
  }

cartWithSingleItemMultipleQuantity : Api.Cart
cartWithSingleItemMultipleQuantity =
  { cartWithSingleItem
  | lines = [ multipleQuantityLineA ]
  , subTotal = multipleQuantityLineA.subTotal
  }

cartWithMultipleItems : Api.Cart
cartWithMultipleItems =
  { cartWithSingleItem
  | lines = [ cartLineA, cartLineB ]
  , subTotal = Helpers.moneyFromInt 11
  }

cartWithMultipleItemsUpdated : Api.Cart
cartWithMultipleItemsUpdated =
  { cartWithSingleItem
  | lines = [ multipleQuantityLineA, cartLineB ]
  , subTotal = Helpers.moneyFromInt 12
  }

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

initTest : Test
initTest =
  describe "Cart.init"
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



updateTest : Test
updateTest =
  describe "Cart.update"
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
            cartState =
              { subTotal = Helpers.moneyFromInt 1
              , items =
                  [ ( cartLineA.productVariant.id
                    , { price = Helpers.moneyFromInt 1
                      , productId = cartLineA.productVariant.product.id
                      , productVariantId = cartLineA.productVariant.id
                      , quantity = 1
                      }
                    )
                  ]
                  |> Dict.fromList
              }
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
                  >> List.member (Cart.Broadcast (CartEvent.CheckoutStarted cartState))
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
            cartChange =
              { subTotal = Helpers.moneyFromInt 1
              , items =
                  [ ( cartLineA.productVariant.id
                    , { price = Helpers.moneyFromInt 1
                      , productId = cartLineA.productVariant.product.id
                      , productVariantId = cartLineA.productVariant.id
                      , quantity = 1
                      }
                    )
                  ]
                  |> Dict.fromList
              }
          in
            Expect.all
              [ Tuple.first
                  >> .cart
                  >> Expect.equal (Cart.Loaded cartWithSingleItemUpdated)
              , Tuple.second
                  >> Expect.equal [ Cart.Broadcast (CartEvent.AddedToCart cartChange) ]
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
            cartChange =
              { subTotal = Helpers.moneyFromInt 1
              , items =
                  [ ( cartLineA.productVariant.id
                    , { price = Helpers.moneyFromInt 1
                      , productId = cartLineA.productVariant.product.id
                      , productVariantId = cartLineA.productVariant.id
                      , quantity = 1
                      }
                    )
                  ]
                  |> Dict.fromList
              }
          in
            Expect.all
              [ Tuple.first
                  >> .cart
                  >> Expect.equal (Cart.Loaded cartWithMultipleItemsUpdated)
              , Tuple.second
                  >> Expect.equal [ Cart.Broadcast (CartEvent.AddedToCart cartChange) ]
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
            updatedLineA =
              { cartLineA
              | quantity = 3
              , subTotal = Helpers.moneyFromInt 3
              }
            updatedCart =
              { cartWithMultipleItems
              | lines = [ updatedLineA, cartLineB ]
              , subTotal = Helpers.moneyFromInt 13
              }
            cartChange =
              { subTotal = Helpers.moneyFromInt 2
              , items =
                  [ ( cartLineA.productVariant.id
                    , { price = Helpers.moneyFromInt 1
                      , productId = cartLineA.productVariant.product.id
                      , productVariantId = cartLineA.productVariant.id
                      , quantity = 2
                      }
                    )
                  ]
                  |> Dict.fromList
              }
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
                  >> List.member (Cart.Broadcast (CartEvent.AddedToCart cartChange))
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
            updatedLineA =
              { cartLineA
              | quantity = 1
              , subTotal = Helpers.moneyFromInt 1
              }
            updatedCart =
              { cartWithSingleItemMultipleQuantity
              | lines = [ updatedLineA ]
              , subTotal = Helpers.moneyFromInt 1
              }
            cartChange =
              { subTotal = Helpers.moneyFromInt 1
              , items =
                  [ ( cartLineA.productVariant.id
                    , { price = Helpers.moneyFromInt 1
                      , productId = cartLineA.productVariant.product.id
                      , productVariantId = cartLineA.productVariant.id
                      , quantity = 1
                      }
                    )
                  ]
                  |> Dict.fromList
              }
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
                  >> List.member (Cart.Broadcast (CartEvent.RemovedFromCart cartChange))
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
              { cartWithMultipleItems
              | lines = [ cartLineB ]
              , subTotal = Helpers.moneyFromInt 10
              }
            cartChange =
              { subTotal = Helpers.moneyFromInt 1
              , items =
                  [ ( cartLineA.productVariant.id
                    , { price = Helpers.moneyFromInt 1
                      , productId = cartLineA.productVariant.product.id
                      , productVariantId = cartLineA.productVariant.id
                      , quantity = 1
                      }
                    )
                  ]
                  |> Dict.fromList
              }
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
                  >> List.member (Cart.Broadcast (CartEvent.RemovedFromCart cartChange))
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
            updatedLineA =
              { cartLineA
              | quantity = 3
              , subTotal = Helpers.moneyFromInt 3
              }
            updatedCart =
              { cartWithMultipleItems
              | lines = [ updatedLineA, cartLineB ]
              , subTotal = Helpers.moneyFromInt 13
              }
            cartChange =
              { subTotal = Helpers.moneyFromInt 2
              , items =
                  [ ( cartLineA.productVariant.id
                    , { price = Helpers.moneyFromInt 1
                      , productId = cartLineA.productVariant.product.id
                      , productVariantId = cartLineA.productVariant.id
                      , quantity = 2
                      }
                    )
                  ]
                  |> Dict.fromList
              }
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
                  >> Expect.equalLists
                      [ Cart.Broadcast (CartEvent.CartCreated updatedCart.id)
                      , Cart.Broadcast (CartEvent.AddedToCart cartChange)
                      ]
              , Tuple.second
                  >> List.member (Cart.Broadcast (CartEvent.CartCreated updatedCart.id))
                  >> Expect.true "Expected list to contain Broadcast CartEvent.CartCreated"
              , Tuple.second
                  >> List.member (Cart.Broadcast (CartEvent.AddedToCart cartChange))
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
            transactionId = "transactionId"
            cartState =
              { subTotal = Helpers.moneyFromInt 11
              , items =
                  [ ( cartLineA.productVariant.id
                    , { price = Helpers.moneyFromInt 1
                      , productId = cartLineA.productVariant.product.id
                      , productVariantId = cartLineA.productVariant.id
                      , quantity = 1
                      }
                    )
                  , ( cartLineB.productVariant.id
                    , { price = Helpers.moneyFromInt 10
                      , productId = cartLineB.productVariant.product.id
                      , productVariantId = cartLineB.productVariant.id
                      , quantity = 1
                      }
                    )
                  ]
                  |> Dict.fromList
              }
            result = modelCartWithMultipleItemsLoaded
              |> Cart.update (Cart.CheckoutCompleted transactionId)
          in
            Expect.all
              [ Tuple.first
                  >> .cart
                  >> Expect.equal Cart.Loading
              , Tuple.second
                  >> List.member (Cart.CreateCart Dict.empty)
                  >> Expect.true "Expected list to contain Cart.CreateCart"
              , Tuple.second
                  >> List.member (Cart.Broadcast (CartEvent.CheckoutCompleted transactionId cartState))
                  >> Expect.true "Expected list to contain Broadcast CartEvent.CheckoutCompleted"
              ]
              result
    ]



diffCartsTest : Test
diffCartsTest =
  describe "Cart.diffCarts"
    [ test """
When old cart is empty
And new cart is empty
Then additions is Nothing
And removals is Nothing
""" <|
        \_ ->
          let
            result = Cart.diffCarts emptyCart emptyCart
          in
            Expect.all
              [ .additions
                  >> Expect.equal Nothing
              , .removals
                  >> Expect.equal Nothing
              ]
              result
    , test """
When old cart is empty
And new cart has single item
Then additions is Just single addition
And removals is Nothing
""" <|
        \_ ->
          let
            cartChange =
              { subTotal = Helpers.moneyFromInt 1
              , items =
                  [ ( cartLineA.productVariant.id
                    , { price = Helpers.moneyFromInt 1
                      , productId = cartLineA.productVariant.product.id
                      , productVariantId = cartLineA.productVariant.id
                      , quantity = 1
                      }
                    )
                  ]
                  |> Dict.fromList
              }
            result = Cart.diffCarts emptyCart cartWithSingleItem
          in
            Expect.all
              [ .additions
                  >> Expect.equal (Just cartChange)
              , .removals
                  >> Expect.equal Nothing
              ]
              result
    , test """
When old cart is empty
And new cart has multiple items
Then additions is Just multiple additions
And removals is Nothing
""" <|
        \_ ->
          let
            cartChange =
              { subTotal = Helpers.moneyFromInt 11
              , items =
                  [ ( cartLineA.productVariant.id
                    , { price = Helpers.moneyFromInt 1
                      , productId = cartLineA.productVariant.product.id
                      , productVariantId = cartLineA.productVariant.id
                      , quantity = 1
                      }
                    )
                  , ( cartLineB.productVariant.id
                    , { price = Helpers.moneyFromInt 10
                      , productId = cartLineB.productVariant.product.id
                      , productVariantId = cartLineB.productVariant.id
                      , quantity = 1
                      }
                    )
                  ]
                  |> Dict.fromList
              }
            result = Cart.diffCarts emptyCart cartWithMultipleItems
          in
            Expect.all
              [ .additions
                  >> Expect.equal (Just cartChange)
              , .removals
                  >> Expect.equal Nothing
              ]
              result
    , test """
When old cart has single item
And new cart has multiple items
Then additions is Just the difference
And removals is Nothing
""" <|
        \_ ->
          let
            cartChange =
              { subTotal = Helpers.moneyFromInt 10
              , items =
                  [ ( cartLineB.productVariant.id
                    , { price = Helpers.moneyFromInt 10
                      , productId = cartLineB.productVariant.product.id
                      , productVariantId = cartLineB.productVariant.id
                      , quantity = 1
                      }
                    )
                  ]
                  |> Dict.fromList
              }
            result = Cart.diffCarts cartWithSingleItem cartWithMultipleItems
          in
            Expect.all
              [ .additions
                  >> Expect.equal (Just cartChange)
              , .removals
                  >> Expect.equal Nothing
              ]
              result
    , test """
When old cart is same as new cart
Then additions is Nothing
And removals is Nothing
""" <|
        \_ ->
          let
            result = Cart.diffCarts cartWithMultipleItems cartWithMultipleItems
          in
            Expect.all
              [ .additions
                  >> Expect.equal Nothing
              , .removals
                  >> Expect.equal Nothing
              ]
              result
    , test """
When old cart has single item
And new cart has empty
Then additions is Nothing
And removals is Just single removal
""" <|
        \_ ->
          let
            cartChange =
              { subTotal = Helpers.moneyFromInt 1
              , items =
                  [ ( cartLineA.productVariant.id
                    , { price = Helpers.moneyFromInt 1
                      , productId = cartLineA.productVariant.product.id
                      , productVariantId = cartLineA.productVariant.id
                      , quantity = 1
                      }
                    )
                  ]
                  |> Dict.fromList
              }
            result = Cart.diffCarts cartWithSingleItem emptyCart
          in
            Expect.all
              [ .additions
                  >> Expect.equal Nothing
              , .removals
                  >> Expect.equal (Just cartChange)
              ]
              result
    , test """
When old cart has single item
And new cart has multiple items
Then additions is Just difference
And removals is Nothing
""" <|
        \_ ->
          let
            cartChange =
              { subTotal = Helpers.moneyFromInt 10
              , items =
                  [ ( cartLineB.productVariant.id
                    , { price = Helpers.moneyFromInt 10
                      , productId = cartLineB.productVariant.product.id
                      , productVariantId = cartLineB.productVariant.id
                      , quantity = 1
                      }
                    )
                  ]
                  |> Dict.fromList
              }
            result = Cart.diffCarts cartWithSingleItem cartWithMultipleItems
          in
            Expect.all
              [ .additions
                  >> Expect.equal (Just cartChange)
              , .removals
                  >> Expect.equal Nothing
              ]
              result
    , test """
When old cart has multiple items
And new cart has empty
Then additions is Nothing
And removals is Just multiple removals
""" <|
        \_ ->
          let
            cartChange =
              { subTotal = Helpers.moneyFromInt 11
              , items =
                  [ ( cartLineA.productVariant.id
                    , { price = Helpers.moneyFromInt 1
                      , productId = cartLineA.productVariant.product.id
                      , productVariantId = cartLineA.productVariant.id
                      , quantity = 1
                      }
                    )
                  , ( cartLineB.productVariant.id
                    , { price = Helpers.moneyFromInt 10
                      , productId = cartLineB.productVariant.product.id
                      , productVariantId = cartLineB.productVariant.id
                      , quantity = 1
                      }
                    )
                  ]
                  |> Dict.fromList
              }
            result = Cart.diffCarts cartWithMultipleItems emptyCart
          in
            Expect.all
              [ .additions
                  >> Expect.equal Nothing
              , .removals
                  >> Expect.equal (Just cartChange)
              ]
              result
    ]
