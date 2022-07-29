port module Cart exposing (..)

import Api
import Browser
import CartEvent exposing (CartEvent)
import Dict exposing (Dict)
import FeatherIcons
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode

-- MAIN

main =
  Browser.element
    { init = initWithCmd
    , update = updateWithCmd
    , subscriptions = always subscriptions
    , view = view
    }

-- PORTS

port events : Encode.Value -> Cmd msg
port commands : Encode.Value -> Cmd msg
port messages : (Encode.Value -> msg) -> Sub msg

-- FLAGS

type alias Flags =
  { endpoint : String
  , token : String
  , cartId : Maybe String
  }

-- EFFECTS

type Effect
  = LoadCart String
  | CreateCart (Dict String Int)
  | CartLinesAdd Api.CartLinesAddInput
  | CartLinesRemove Api.CartLinesRemoveInput
  | CartLinesUpdate Api.CartLinesUpdateInput
  | StartCheckout String
  | Broadcast CartEvent
  | Noop

effectToCmd : Api.Config -> Effect -> Cmd Msg
effectToCmd apiConfig effect =
  case effect of
    LoadCart cartId ->
      loadCart apiConfig cartId

    CreateCart lines ->
      createCart apiConfig lines

    CartLinesAdd input ->
      cartLinesAdd apiConfig input

    CartLinesRemove input ->
      cartLinesRemove apiConfig input

    CartLinesUpdate input ->
      cartLinesUpdate apiConfig input

    StartCheckout url ->
      Encode.object
        [ ( "type", Encode.string "Checkout" )
        , ( "url" , Encode.string url )
        ]
        |> commands

    Broadcast event ->
      CartEvent.encode event |> events

    Noop -> Cmd.none

cartApiResultToMsg : (a -> Msg) -> Msg -> Cmd (Result x (Maybe a)) -> Cmd Msg
cartApiResultToMsg happyMapper unhappyMsg cmd =
  cmd
    |> Cmd.map (Result.toMaybe)
    -- flatten maybes
    |> Cmd.map (Maybe.andThen identity)
    |> Cmd.map (Maybe.map happyMapper)
    |> Cmd.map (Maybe.withDefault unhappyMsg)

loadCart : Api.Config -> String -> Cmd Msg
loadCart config cartId =
  Api.getCart config cartId
    |> cartApiResultToMsg CartLoaded CartLoadingFailed

createCart : Api.Config -> Dict String Int -> Cmd Msg
createCart config lines =
  Api.createCart config lines
    |> cartApiResultToMsg CartCreated CartCreationFailed

cartLinesAdd : Api.Config -> Api.CartLinesAddInput -> Cmd Msg
cartLinesAdd config input =
  Api.cartLinesAdd config input
    |> cartApiResultToMsg CartUpdated CartUpdateFailed

cartLinesRemove : Api.Config -> Api.CartLinesRemoveInput -> Cmd Msg
cartLinesRemove config input =
  Api.cartLinesRemove config input
    |> cartApiResultToMsg CartUpdated CartUpdateFailed

cartLinesUpdate : Api.Config -> Api.CartLinesUpdateInput -> Cmd Msg
cartLinesUpdate config input =
  Api.cartLinesUpdate config input
    |> cartApiResultToMsg CartUpdated CartUpdateFailed

-- RemoteCart

type RemoteCart
  = Loading
  | Loaded Api.Cart
  | CreationFailed
  | Updating Api.Cart (String, Int)
  | Recreating Api.Cart (String, Int)

getCart : RemoteCart -> Maybe Api.Cart
getCart remoteCart =
  case remoteCart of
    Loading ->
      Nothing
    Loaded cart ->
      Just cart
    CreationFailed ->
      Nothing
    Updating cart change ->
      Just cart
    Recreating cart change ->
      Just cart

---

type UIState
  = Open
  | Closed

-- MODEL

type alias Model =
  { apiConfig : Api.Config
  , cart : RemoteCart
  , uiState : UIState
  }

-- INIT

initWithCmd : Flags -> (Model, Cmd Msg)
initWithCmd flags =
  let
    (cart, effect) = init flags
  in
    ( cart
    , effectToCmd cart.apiConfig effect
    )

init : Flags -> (Model, Effect)
init { endpoint, token, cartId } =
  let
    effect = cartId
      |> Maybe.map LoadCart
      |> Maybe.withDefault (CreateCart Dict.empty)
  in
  ( { apiConfig =
        { url = endpoint
        , token = token
        }
    , cart = Loading
    , uiState = Closed
    }
  , effect
  )

-- MESSAGES

type Msg
  = NoOpMsg
  | OpenCart
  | CloseCart
  | CartCreated Api.Cart
  | CartCreationFailed
  | CartLoaded Api.Cart
  | CartLoadingFailed
  | CartUpdated Api.Cart
  | CartUpdateFailed
  | UpdateCart String Int
  | AddToCart String
  | Checkout
  | CheckoutCompleted

addToCart : String -> Msg
addToCart productVariantId =
  AddToCart productVariantId

checkout : Msg
checkout =
  Checkout

removeFromCart : String -> Msg
removeFromCart productVariantId =
  UpdateCart productVariantId 0

updateItemQuantityInCart : String -> Int -> Msg
updateItemQuantityInCart productVariantId quantity =
  UpdateCart productVariantId quantity

-- UPDATE

updateWithCmd : Msg -> Model -> (Model, Cmd Msg)
updateWithCmd msg model =
  let
    (cart, effect) = update msg model
  in
    ( cart
    , effectToCmd cart.apiConfig effect
    )

update : Msg -> Model -> (Model, Effect)
update msg model =
  case msg of
    NoOpMsg -> (model, Noop)
    OpenCart ->
      ( { model | uiState = Open }
      , Noop
      )
    CloseCart ->
      ( { model | uiState = Closed }
      , Noop
      )
    CartCreated cart ->
      ( { model | cart = Loaded cart }
      , Broadcast (CartEvent.CartCreated cart.id)
      )
    CartCreationFailed ->
      ( { model | cart = CreationFailed }
      , Noop
      )
    CartLoaded cart ->
      ( { model | cart = Loaded cart }
      , Noop
      )
    CartLoadingFailed ->
      ( { model | cart = Loading }
      , CreateCart Dict.empty
      )
    AddToCart productVariantId ->
      case model.cart of
        Loaded cart ->
          let
            newQuantity = (quantityOfProductVariant cart productVariantId) + 1
          in
            updateCart model cart productVariantId newQuantity
        _ -> (model, Noop)
    UpdateCart productVariantId newQuantity ->
      case model.cart of
        Loaded cart ->
          updateCart model cart productVariantId newQuantity
        _ -> (model, Noop)
    CartUpdated cart ->
      ( { model | cart = Loaded cart }
      , Noop
      )
    CartUpdateFailed ->
      case model.cart of
        Updating cart change ->
          ( { model | cart = Recreating cart change }
          , CreateCart <| calculateCreateCartChange cart change
          )
        _ -> (model, Noop)
    Checkout ->
      case model.cart of
        Loaded cart ->
          let
            quantity = cart.lines
              |> List.map .quantity
              |> List.sum
            effect =
              if quantity > 0 then
                StartCheckout cart.checkoutUrl
              else
                Noop
          in
            (model, effect)
        _ -> (model, Noop)
    CheckoutCompleted ->
      ( { model | cart = Loading }
      , CreateCart Dict.empty
      )

updateCart : Model -> Api.Cart -> String -> Int -> (Model, Effect)
updateCart model cart productVariantId quantity =
  ( { model | cart = Updating cart (productVariantId, quantity) }
  , calculateCartUpdateEffect cart (productVariantId, quantity)
  )

quantityOfProductVariant : Api.Cart -> String -> Int
quantityOfProductVariant cart productVariantId =
  cart.lines
    |> List.map getProductVariantIdAndQuantity
    |> Dict.fromList
    |> Dict.get productVariantId
    |> Maybe.withDefault 0

calculateCartUpdateEffect : Api.Cart -> (String, Int) -> Effect
calculateCartUpdateEffect cart (productVariantId, quantity) =
  let
    lineIdToEffect =
      if quantity > 0 then
        (\ lineId ->
          CartLinesUpdate
            { cartId = cart.id
            , lineId = lineId
            , quantity = quantity
            }
        )
      else
        (\ lineId ->
          CartLinesRemove
            { cartId = cart.id
            , lineId = lineId
            }
        )
  in
    cart.lines
      |> List.map getProductVariantIdAndLineId
      |> Dict.fromList
      |> Dict.get productVariantId
      |> Maybe.map lineIdToEffect
      |> Maybe.withDefault
          ( CartLinesAdd
            { cartId = cart.id
            , productVariantId = productVariantId
            , quantity = quantity
            }
          )

getProductVariantIdAndLineId : Api.CartLine -> (String, String)
getProductVariantIdAndLineId line =
  ( line.productVariant.id
  , line.id
  )

getProductVariantIdAndQuantity : Api.CartLine -> (String, Int)
getProductVariantIdAndQuantity line =
  (line.productVariant.id, line.quantity)

moreThan : Int -> Int -> Bool
moreThan compare value =
  value > compare

calculateCreateCartChange : Api.Cart -> (String, Int) -> Dict String Int
calculateCreateCartChange cart (id, quantity) =
  cart.lines
    |> List.map getProductVariantIdAndQuantity
    |> Dict.fromList
    |> Dict.insert id quantity
    |> Dict.filter (always (moreThan 0))


-- SUBSCRIPTIONS

subscriptions : Sub Msg
subscriptions =
  Decode.decodeValue messageDecoder
    >> Result.withDefault NoOpMsg
    |> messages

decodeTypeField : String -> Decoder Msg
decodeTypeField value =
  case value of
    "CheckoutCompleted" ->
      Decode.succeed CheckoutCompleted
    _ ->
      Decode.fail <| "Unknown value '" ++ value ++ "' for field 'type'"

messageDecoder : Decoder Msg
messageDecoder =
  Decode.field "type" Decode.string
    |> Decode.andThen decodeTypeField


-- VIEW

view : Model -> Html Msg
view model =
  let
    cartContent =
       case model.uiState of
         Closed ->
           remoteCartView model.cart False
         Open ->
           remoteCartView model.cart True
  in
    H.div [ A.class "avenir", A.id "cart" ]
      [ shopView (getCart model.cart)
      , cartContent
      ]

shopView : Maybe Api.Cart -> Html Msg
shopView cart =
  H.section [ A.class "" ]
    [ cartButton cart
    , H.h1 [ A.class "f2 fw4" ] [ H.text "Shop View" ]
    , H.h2 [ A.class "f3 fw4"] [ H.text "Finn Lamp Shade" ]
    , button [ E.onClick (addToCart finnID) ] [ H.text "Add to cart"]
    , H.h2 [ A.class "f3 fw4"] [ H.text "Light Sculpture" ]
    , button [ E.onClick (addToCart lightSculptureID) ] [ H.text "Add to cart"]
    , H.h2 [ A.class "f3 fw4"] [ H.text "Test Product" ]
    , button [ E.onClick (addToCart testProductID) ] [ H.text "Add to cart"]
    ]

-- TODO: Add manual recovery from CreationFailed
remoteCartView : RemoteCart -> Bool -> Html Msg
remoteCartView remoteCart isOpen =
  let
    transformAttribute =
      if isOpen then
        A.style "transform" "translateX(0%)"
      else
        A.style "transform" "translateX(100%)"
    content =
      case remoteCart of
        Loading ->
          H.p [] [ H.text "Loading" ]
        Loaded cart ->
          cartView cart
        CreationFailed ->
          H.p [] [ H.text "Error" ]
        Updating cart change ->
          cartView cart
        Recreating cart change ->
          cartView cart
  in
    H.section
      [ A.class "fixed right-0 top-0 h-100 measure w-100 bg-white shadow-2"
      , transformAttribute
      , A.style "transition" "transform .25s ease-in-out"
      ]
      [ H.div [ A.class "flex flex-column h-100 justify-between"]
          [ cartHeader
          , content
          , cartFooter (getCart remoteCart)
          ]
      ]

cartHeader : Html Msg
cartHeader =
  H.div [ A.class "self-start w-100 ph3 flex justify-between" ]
    [ H.h1 [ A.class "f3 fw4" ] [ H.text "Cart" ]
    , closeButton
    ]

-- TODO: Disable checkout button if cart is empty
cartFooter : Maybe Api.Cart -> Html Msg
cartFooter maybeCart =
  let
    content =
      case maybeCart of
        Just cart ->
          [ H.p [ A.class "" ]
              [ H.span [ A.class "" ] [ H.text "Subtotal:" ]
              , H.span [ A.class "fr fw6" ] [ H.text <| formatGBP cart.subTotal.amount ]
              ]
          , H.p [ A.class "tc f6" ] [ H.text "Shipping and discount codes are added at checkout." ]
          , button
              [ A.class "db w-100"
              , E.onClick checkout
              ]
              [ H.text "Checkout" ]
          ]
        Nothing ->
          []
  in
    H.div [ A.class "self-end w-100 ph3"] content



cartView : Api.Cart -> Html Msg
cartView cart =
  let
    content =
      if (List.length cart.lines) > 0 then
        List.map lineView cart.lines
      else
        [ H.p [] [ H.text "Your cart is empty." ] ]
  in
    H.div [ A.class "h-100 overflow-y-scroll ph3" ] content

imageView : Api.Image -> Html msg
imageView image =
  H.img
    [ A.class "db mw-100 h-auto bg-light-gray"
    , A.src image.url
    , A.width 64
    , A.height 64
    ]
    []

-- TODO: disable + button if quantity available is reached
lineView : Api.CartLine -> Html Msg
lineView { id, productVariant, quantity, subTotal } =
  let
    image = productVariant.image
      |> Maybe.map imageView
      |> Maybe.withDefault
          (H.div [ A.class "dib w3 h3 bg-light-gray" ] [])
  in
    H.div [ A.class "flex mb3" ]
      [ H.span [ A.class "db pa0" ] [ image ]
      , H.div [ A.class "ml2 flex-auto"]
          [ H.h4 [ A.class "mv1"] [ H.text productVariant.product.title ]
          , H.div [ A.class "dib" ]
              [ H.a
                  [ A.class "ba br2 br--left pa2 dim pointer dib w2 tc b bg-white"
                  , E.onClick (updateItemQuantityInCart productVariant.id (quantity - 1))
                  ]
                  [ H.text "–"]
              , H.span [ A.class "bt bb pa2 dib w2 tc" ] [ H.text <| String.fromInt quantity ]
              , H.a
                  [ A.class "ba br2 br--right pa2 dim pointer dib w2 tc b bg-white"
                  , E.onClick (updateItemQuantityInCart productVariant.id (quantity + 1))
                  ]
                  [ H.text "+"]
              ]
          , H.span [ A.class "dib fr fw6 pv2" ] [ H.text <| formatGBP subTotal.amount ]
          ]
      ]

-- VIEW COMPONENTS

button : List (Attribute msg) -> List (Html msg) -> Html msg
button attributes =
  let
    newAttributes =
      List.append
        [ A.class "f5 link dim mb2 bn br3 ph4 pv3 white bg-black pointer" ]
        attributes
  in
    H.button newAttributes

closeButton : Html Msg
closeButton =
  H.button
    [ A.class "f1 bn pointer dim ph0 bg-white"
    , E.onClick CloseCart
    ]
    -- TODO: Add aria attributes for screen readers
    [ H.text "×"]

totalItemsOfCart : Api.Cart -> Int
totalItemsOfCart cart =
  cart.lines
    |> List.map .quantity
    |> List.sum

cartButton : Maybe Api.Cart -> Html Msg
cartButton cart =
  let
    totalItemsText =
      cart
        |> Maybe.map totalItemsOfCart
        |> Maybe.map (String.fromInt >> H.text)
        |> Maybe.withDefault (H.text "...")
  in
    H.div
      [ A.class "fixed top-0 right-0 h-100 flex items-center" ]
      [ H.a
          [ A.class "f6 link dim mb2 bt bl bb b--white br2 br--left ph3 pv2 white bg-black pointer"
          , E.onClick OpenCart
          ]
          [ H.span
              [ A.class "db tc mb2" ]
              [ totalItemsText ]
          , H.span
              [ A.class "db tc" ]
              [ FeatherIcons.shoppingCart
                  |> FeatherIcons.withSize 1.5
                  |> FeatherIcons.withSizeUnit "em"
                  |> FeatherIcons.toHtml []
              ]
          ]
      ]

-- UTILS

formatGBP : String -> String
formatGBP value =
  "£" ++ value ++ "0"

-- TEMPORARY

finnID : String
-- finnID = "7260086239427"
-- variant gid://shopify/ProductVariant/41819646623939
finnID = "gid://shopify/ProductVariant/41819646623939"

lightSculptureID : String
-- lightSculptureID = "7298460090563"
-- variant gid://shopify/ProductVariant/41915944763587
lightSculptureID = "gid://shopify/ProductVariant/41915944763587"

testProductID : String
testProductID = "gid://shopify/ProductVariant/41980294922435"

-- gid://shopify/Cart/b7d0ca2201fc39f09818b9466ccc1a00

-- cart gid://shopify/Cart/1f96945c6997807c2b66812b8d102027
-- gid://shopify/Cart/41578ab8c241781a654823d14d75776a

-- cart with 10 Finn, 1 Light Sculpture
-- gid://shopify/Cart/546afca7e868f12bc644b9af3057bd0f


-- note: Adding more products than available when creating a cart will not add
-- them to the cart (they max out)
