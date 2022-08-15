module Helpers exposing (..)

import Decimal
import Fuzz exposing (Fuzzer)
import Random

import Api
import Cart
import CartEvent exposing (CartEvent)
import Money exposing (Money)

-- Util

moneyFromInt : Int -> Money
moneyFromInt value =
  { amount = Decimal.fromInt value
  , currencyCode = "GBP"
  }

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
    (Fuzz.constant (moneyFromInt 0))

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

moneyFuzzer : Fuzzer Money
moneyFuzzer =
  Fuzz.map2 Money
    (Fuzz.int |> Fuzz.map Decimal.fromInt)
    (Fuzz.constant "GBP")

productFuzzer : Fuzzer Api.Product
productFuzzer =
  Fuzz.map2 Api.Product
    Fuzz.string
    Fuzz.string

productVariantFuzzer : Fuzzer Api.ProductVariant
productVariantFuzzer =
  Fuzz.map5 Api.ProductVariant
    Fuzz.string
    (Fuzz.maybe imageFuzzer)
    moneyFuzzer
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
