module Api exposing (..)

import Decimal exposing (Decimal)
import Dict exposing (Dict)
import Graphql.Document as Document
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Money exposing (Money)
import Shopify.Enum.CurrencyCode as CurrencyCode
import Shopify.InputObject as InputObject
import Shopify.Mutation as Mutation
import Shopify.Object
import Shopify.Object.Cart as Cart
import Shopify.Object.CartCost as CartCost
import Shopify.Object.CartCreatePayload as CartCreatePayload
import Shopify.Object.CartLine as CartLine
import Shopify.Object.CartLineConnection as CartLineConnection
import Shopify.Object.CartLineCost as CartLineCost
import Shopify.Object.CartLinesAddPayload as CartLinesAddPayload
import Shopify.Object.CartLinesRemovePayload as CartLinesRemovePayload
import Shopify.Object.CartLinesUpdatePayload as CartLinesUpdatePayload
import Shopify.Object.Image as ShopifyImage
import Shopify.Object.MoneyV2 as MoneyV2
import Shopify.Object.Product as Product
import Shopify.Object.ProductVariant as ProductVariant
import Shopify.Query as Query
import Shopify.Scalar
import Shopify.ScalarCodecs
import Shopify.Union
import Shopify.Union.Merchandise as Merchandise



-- Utils

transformDecimal : Shopify.ScalarCodecs.Decimal -> Result String Decimal
transformDecimal value =
  let
    stringValue = value
      |> unwrapDecimal
  in
    stringValue
      |> Decimal.fromString
      |> Result.fromMaybe ("Unable to parse \"" ++ stringValue ++ "\" as Decimal")

unwrapDecimal : Shopify.ScalarCodecs.Decimal -> String
unwrapDecimal wrapped =
  case wrapped of
    Shopify.Scalar.Decimal value -> value

unwrapId : Shopify.ScalarCodecs.Id -> String
unwrapId wrapped =
  case wrapped of
    Shopify.Scalar.Id value -> value

unwrapUrl : Shopify.ScalarCodecs.Url -> String
unwrapUrl wrapped =
  case wrapped of
    Shopify.Scalar.Url value -> value

-- Model

type alias Cart =
  { id : String
  , checkoutUrl : String
  , lines : List CartLine
  , subTotal : Money
  }

type alias CartLine =
  { id : String
  , productVariant : ProductVariant
  , quantity : Int
  , subTotal : Money
  }

type alias Config =
  { url : String
  , token : String
  }

type alias Product =
    { id : String
    , title : String
    }

type alias Image =
  { url : String
  }

type alias ProductVariant =
  { id : String
  , image : Maybe Image
  , price : Money
  , product : Product
  , quantityAvailable : Int
  }



-- SelectionSets

cartLinesFirstTen : SelectionSet decodesTo Shopify.Object.CartLineConnection -> SelectionSet decodesTo Shopify.Object.Cart
cartLinesFirstTen =
  Cart.lines
    (\ optionals ->
      { optionals |
          first = Present 10
      }
    )

merchandiseSelection : SelectionSet ProductVariant Shopify.Union.Merchandise
merchandiseSelection =
  Merchandise.fragments { onProductVariant = productVariantSelection }

cartLineCostSelection : SelectionSet Money Shopify.Object.CartLineCost
cartLineCostSelection =
  SelectionSet.succeed identity
    |> SelectionSet.with (CartLineCost.subtotalAmount moneyV2Selection)

lineInfoSelection : SelectionSet CartLine Shopify.Object.CartLine
lineInfoSelection =
  SelectionSet.succeed CartLine
    |> SelectionSet.with (SelectionSet.map unwrapId CartLine.id)
    |> SelectionSet.with (CartLine.merchandise merchandiseSelection)
    |> SelectionSet.with CartLine.quantity
    |> SelectionSet.with (CartLine.cost cartLineCostSelection)

linesInfoSelection : SelectionSet (List CartLine) Shopify.Object.CartLineConnection
linesInfoSelection =
  CartLineConnection.nodes lineInfoSelection

moneyV2Selection : SelectionSet Money Shopify.Object.MoneyV2
moneyV2Selection =
  SelectionSet.succeed Money
    |> SelectionSet.with (SelectionSet.mapOrFail transformDecimal MoneyV2.amount)
    |> SelectionSet.with (SelectionSet.map CurrencyCode.toString MoneyV2.currencyCode)

cartCostSelection : SelectionSet Money Shopify.Object.CartCost
cartCostSelection =
  SelectionSet.succeed identity
    |> SelectionSet.with (CartCost.subtotalAmount moneyV2Selection)

cartInfoSelection : SelectionSet Cart Shopify.Object.Cart
cartInfoSelection =
    SelectionSet.succeed Cart
      |> SelectionSet.with (SelectionSet.map unwrapId Cart.id)
      |> SelectionSet.with (SelectionSet.map unwrapUrl Cart.checkoutUrl)
      |> SelectionSet.with (cartLinesFirstTen linesInfoSelection)
      |> SelectionSet.with (Cart.cost cartCostSelection)

productInfoSelection : SelectionSet Product Shopify.Object.Product
productInfoSelection =
    SelectionSet.succeed Product
        |> SelectionSet.with (SelectionSet.map unwrapId Product.id)
        |> SelectionSet.with Product.title

productVariantSelection : SelectionSet ProductVariant Shopify.Object.ProductVariant
productVariantSelection =
  SelectionSet.succeed ProductVariant
    |> SelectionSet.with (SelectionSet.map unwrapId ProductVariant.id)
    |> SelectionSet.with (ProductVariant.image <| imageSelection 128 128)
    |> SelectionSet.with (ProductVariant.priceV2 moneyV2Selection)
    |> SelectionSet.with (ProductVariant.product productInfoSelection)
    |> SelectionSet.with (SelectionSet.map (Maybe.withDefault 0) ProductVariant.quantityAvailable)

imageSelection : Int -> Int -> SelectionSet Image Shopify.Object.Image
imageSelection maxWidth maxHeight =
  let
    imageTransformInput = InputObject.buildImageTransformInput
      (\ optionals ->
        { optionals |
          maxWidth = Present maxWidth
        , maxHeight = Present maxHeight
        }
      )
    selectUrl = ShopifyImage.url
      (\ optionals ->
        { optionals |
          transform = Present imageTransformInput
        }
      )
  in
    SelectionSet.succeed Image
      |> SelectionSet.with (SelectionSet.map unwrapUrl selectUrl)

-- Queries/Mutations

cartLineInput : (String, Int) -> InputObject.CartLineInput
cartLineInput (productVariantId, quantity) =
  InputObject.buildCartLineInput
      { merchandiseId = Shopify.Scalar.Id productVariantId }
      (\ optionals ->
        { optionals |
            quantity = Present quantity
        }
      )

createCartMutation : Dict String Int -> SelectionSet (Maybe Cart) RootMutation
createCartMutation linesDict =
  let
    lines = linesDict
      |> Dict.toList
      |> List.map cartLineInput
    cartInput =
      (\ optionals ->
        { optionals |
            lines = Present lines
        }
      )
  in
    Mutation.cartCreate
      (\ optionals ->
        { optionals |
            input = Present (InputObject.buildCartInput cartInput)
        }
      )
      (CartCreatePayload.cart cartInfoSelection)
      -- flatten maybes
      |> SelectionSet.map (Maybe.andThen identity)

createCart : Config -> Dict String Int -> Cmd (Result (Graphql.Http.Error (Maybe Cart)) (Maybe Cart))
createCart { url, token } lines =
    createCartMutation lines
        |> Graphql.Http.mutationRequest url
        |> Graphql.Http.withHeader "X-Shopify-Storefront-Access-Token" token
        |> Graphql.Http.send identity

getCartQuery : String -> SelectionSet (Maybe Cart) RootQuery
getCartQuery id =
    Query.cart
        { id = Shopify.Scalar.Id id }
        cartInfoSelection

-- could make this nicer and return an actual cart
-- or error when cart not found
getCart : Config -> String -> Cmd (Result (Graphql.Http.Error (Maybe Cart)) (Maybe Cart))
getCart { url, token } id =
    getCartQuery id
        |> Graphql.Http.queryRequest url
        |> Graphql.Http.withHeader "X-Shopify-Storefront-Access-Token" token
        |> Graphql.Http.send identity

type alias CartLinesAddInput =
  { cartId : String
  , productVariantId : String
  , quantity : Int
  }

cartLinesAddMutation : CartLinesAddInput -> SelectionSet (Maybe Cart) RootMutation
cartLinesAddMutation { cartId, productVariantId, quantity } =
    Mutation.cartLinesAdd
      { cartId = Shopify.Scalar.Id cartId
      , lines = [ cartLineInput (productVariantId, quantity) ]
      }
      (CartLinesAddPayload.cart cartInfoSelection)
      -- flatten maybes
      |> SelectionSet.map (Maybe.andThen identity)

cartLinesAdd : Config -> CartLinesAddInput -> Cmd (Result (Graphql.Http.Error (Maybe Cart)) (Maybe Cart))
cartLinesAdd { url, token } input =
    cartLinesAddMutation input
        |> Graphql.Http.mutationRequest url
        |> Graphql.Http.withHeader "X-Shopify-Storefront-Access-Token" token
        |> Graphql.Http.send identity

type alias CartLinesRemoveInput =
  { cartId : String
  , lineId : String
  }

cartLinesRemoveMutation : CartLinesRemoveInput -> SelectionSet (Maybe Cart) RootMutation
cartLinesRemoveMutation { cartId, lineId } =
    Mutation.cartLinesRemove
      { cartId = Shopify.Scalar.Id cartId
      , lineIds = [ Shopify.Scalar.Id lineId ]
      }
      (CartLinesRemovePayload.cart cartInfoSelection)
      -- flatten maybes
      |> SelectionSet.map (Maybe.andThen identity)

cartLinesRemove : Config -> CartLinesRemoveInput -> Cmd (Result (Graphql.Http.Error (Maybe Cart)) (Maybe Cart))
cartLinesRemove { url, token } input =
    cartLinesRemoveMutation input
        |> Graphql.Http.mutationRequest url
        |> Graphql.Http.withHeader "X-Shopify-Storefront-Access-Token" token
        |> Graphql.Http.send identity

type alias CartLinesUpdateInput =
  { cartId : String
  , lineId : String
  , quantity : Int
  }

cartLinesUpdateMutation : CartLinesUpdateInput -> SelectionSet (Maybe Cart) RootMutation
cartLinesUpdateMutation { cartId, lineId, quantity } =
    Mutation.cartLinesUpdate
      { cartId = Shopify.Scalar.Id cartId
      , lines =
          [ InputObject.buildCartLineUpdateInput
              { id = Shopify.Scalar.Id lineId }
              (\ optionals ->
                { optionals |
                    quantity = Present quantity
                }
              )
          ]
      }
      (CartLinesUpdatePayload.cart cartInfoSelection)
      -- flatten maybes
      |> SelectionSet.map (Maybe.andThen identity)

cartLinesUpdate : Config -> CartLinesUpdateInput -> Cmd (Result (Graphql.Http.Error (Maybe Cart)) (Maybe Cart))
cartLinesUpdate { url, token } input =
    cartLinesUpdateMutation input
        |> Graphql.Http.mutationRequest url
        |> Graphql.Http.withHeader "X-Shopify-Storefront-Access-Token" token
        |> Graphql.Http.send identity
