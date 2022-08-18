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
import Money exposing (Money)

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
  , productVariantId : Maybe String
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
  | CreationFailed -- No cart was loaded and we tried to create one
  | Updating Api.Cart Change
  | Recreating Api.Cart Change
  | RecreationFailed Api.Cart Change -- We had a cart and needed to recreate it

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
    RecreationFailed cart change ->
      Just cart

getCartWithChange : RemoteCart -> Maybe (Api.Cart, Change)
getCartWithChange remoteCart =
  case remoteCart of
    Loading ->
      Nothing
    Loaded cart ->
      Nothing
    CreationFailed ->
      Nothing
    Updating cart change ->
      Just (cart, change)
    Recreating cart change ->
      Just (cart, change)
    RecreationFailed cart change ->
      Just (cart, change)

type alias Change =
  (String, Int)

---

type UIState
  = Open
  | Closed

-- MODEL

type alias Model =
  { apiConfig : Api.Config
  , cart : RemoteCart
  , uiState : UIState
  , productVariantId : Maybe String
  }

-- INIT

initWithCmd : Flags -> (Model, Cmd Msg)
initWithCmd flags =
  let
    (cart, effects) = init flags
    cmd = effects
      |> List.map (effectToCmd cart.apiConfig)
      |> Cmd.batch
  in
    ( cart, cmd )

init : Flags -> (Model, List Effect)
init { endpoint, token, cartId, productVariantId } =
  let
    effects = cartId
      |> Maybe.map LoadCart
      |> Maybe.withDefault (CreateCart Dict.empty)
      |> List.singleton
    maybeProductVariantId = productVariantId
      |> Maybe.map (String.append "gid://shopify/ProductVariant/")
  in
  ( { apiConfig =
        { url = endpoint
        , token = token
        }
    , cart = Loading
    , uiState = Closed
    , productVariantId = maybeProductVariantId
    }
  , effects
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
  | CheckoutCompleted String
  | Retry

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
    (cart, effects) = update msg model
    cmd = effects
      |> List.map (effectToCmd cart.apiConfig)
      |> Cmd.batch
  in
    ( cart, cmd )

update : Msg -> Model -> (Model, List Effect)
update msg model =
  case msg of

    NoOpMsg -> (model, [])

    OpenCart ->
      ( { model | uiState = Open }
      , []
      )

    CloseCart ->
      ( { model | uiState = Closed }
      , []
      )

    CartCreated cart ->
      ( { model | cart = Loaded cart }
      , List.append
          [ Broadcast (CartEvent.CartCreated cart.id) ]
          (calculateCartChangeEvent model.cart cart)
      )

    CartCreationFailed ->
      let
        remoteCart = model.cart
          |> getCartWithChange
          |> Maybe.map (\ (cart, change) -> RecreationFailed cart change )
          |> Maybe.withDefault CreationFailed
      in
        ( { model | cart = remoteCart }
        , []
        )

    CartLoaded cart ->
      ( { model | cart = Loaded cart }
      , []
      )

    CartLoadingFailed ->
      ( { model | cart = Loading }
      , [ CreateCart Dict.empty ]
      )

    AddToCart productVariantId ->
      case model.cart of
        Loaded cart ->
          let
            newQuantity = (quantityOfProductVariant cart productVariantId) + 1
          in
            updateCart model cart productVariantId newQuantity
        _ -> (model, [])

    UpdateCart productVariantId newQuantity ->
      case model.cart of
        Loaded cart ->
          updateCart model cart productVariantId newQuantity
        _ -> (model, [])

    CartUpdated cart ->
      ( { model | cart = Loaded cart }
      , calculateCartChangeEvent model.cart cart
      )

    CartUpdateFailed ->
      case model.cart of
        Updating cart change ->
          ( { model | cart = Recreating cart change }
          , [ CreateCart <| calculateCreateCartChange cart change ]
          )
        _ -> (model, [])

    Checkout ->
      case model.cart of
        Loaded cart ->
          let
            quantity = cartItemCount cart
            cartState =
              { subTotal = cart.subTotal
              , items = cartToCartItems cart
              }
            effects =
              if quantity > 0 then
                [ StartCheckout cart.checkoutUrl
                , Broadcast (CartEvent.CheckoutStarted cartState)
                ]
              else
                []
          in
            (model, effects)
        _ -> (model, [])

    CheckoutCompleted transactionId ->
      case getCart model.cart of
        Just cart ->
          let
            cartState =
              { subTotal = cart.subTotal
              , items = cartToCartItems cart
              }
          in
            ( { model | cart = Loading }
            , [ CreateCart Dict.empty
              , Broadcast (CartEvent.CheckoutCompleted transactionId cartState )
              ]
            )
        Nothing ->
          ( { model | cart = Loading }
          , [ CreateCart Dict.empty ]
          )

    Retry ->
      case model.cart of
        CreationFailed ->
          ( { model | cart = Loading }
          , [ CreateCart Dict.empty ]
          )
        RecreationFailed cart change ->
          ( { model | cart = Recreating cart change }
          , [ CreateCart <| calculateCreateCartChange cart change ]
          )
        _ -> ( model, [] )

updateCart : Model -> Api.Cart -> String -> Int -> (Model, List Effect)
updateCart model cart productVariantId quantity =
  ( { model | cart = Updating cart (productVariantId, quantity) }
  , [ calculateCartUpdateEffect cart (productVariantId, quantity) ]
  )

quantityOfProductVariant : Api.Cart -> String -> Int
quantityOfProductVariant cart productVariantId =
  cart.lines
    |> List.map getProductVariantIdAndQuantity
    |> Dict.fromList
    |> Dict.get productVariantId
    |> Maybe.withDefault 0

calculateCartUpdateEffect : Api.Cart -> Change -> Effect
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

calculateCreateCartChange : Api.Cart -> Change -> Dict String Int
calculateCreateCartChange cart (id, quantity) =
  cart.lines
    |> List.map getProductVariantIdAndQuantity
    |> Dict.fromList
    |> Dict.insert id quantity
    |> Dict.filter (always (moreThan 0))

cartItemCount : Api.Cart -> Int
cartItemCount cart =
  cart.lines
    |> List.map .quantity
    |> List.sum

type alias CartDiff =
  { additions : Maybe CartEvent.CartChange
  , removals : Maybe CartEvent.CartChange
  }

cartToCartItems : Api.Cart -> Dict String CartEvent.CartItem
cartToCartItems cart =
  cart.lines
    |> List.map (\ line ->
        ( line.productVariant.id
        , { price = line.productVariant.price
          , productId = line.productVariant.product.id
          , productVariantId = line.productVariant.id
          , quantity = line.quantity
          }
        )
      )
    |> Dict.fromList

emptyCartDiff : CartDiff
emptyCartDiff =
  { additions = Nothing
  , removals = Nothing
  }

cartChangeFromCartItem : CartEvent.CartItem -> CartEvent.CartChange
cartChangeFromCartItem item =
  { subTotal = Money.mul item.price item.quantity
  , items = Dict.singleton item.productVariantId item
  }

addDiffToExistingChange : CartEvent.CartItem -> CartEvent.CartChange -> CartEvent.CartChange
addDiffToExistingChange newItem existingChange =
  { subTotal = Money.add existingChange.subTotal (Money.mul newItem.price newItem.quantity)
  , items = Dict.insert newItem.productVariantId newItem existingChange.items
  }

diffCarts : Api.Cart -> Api.Cart -> CartDiff
diffCarts oldCart newCart =
  Dict.merge
    -- left
    (\ _ vl acc ->
      { acc | removals =
          acc.removals
            |> Maybe.map (addDiffToExistingChange vl)
            |> Maybe.withDefault (cartChangeFromCartItem vl)
            |> Just
      }
    )
    -- both
    (\ _ vl vr acc ->
      if vl.quantity > vr.quantity then
        -- removals
        let
          item =
            { price = vl.price
            , productId = vl.productId
            , productVariantId = vl.productVariantId
            , quantity = vl.quantity - vr.quantity
            }
          removals =
            acc.removals
              |> Maybe.map (addDiffToExistingChange item)
              |> Maybe.withDefault (cartChangeFromCartItem item)
              |> Just
        in
          { acc | removals = removals }
      else if vl.quantity < vr.quantity then
        -- additions
        let
          item =
            { price = vr.price
            , productId = vr.productId
            , productVariantId = vr.productVariantId
            , quantity = vr.quantity - vl.quantity
            }
          additions =
            acc.additions
              |> Maybe.map (addDiffToExistingChange item)
              |> Maybe.withDefault (cartChangeFromCartItem item)
              |> Just
        in
          { acc | additions = additions }
      else
        acc
    )
    -- right
    (\ _ vr acc ->
      { acc | additions =
          acc.additions
            |> Maybe.map (addDiffToExistingChange vr)
            |> Maybe.withDefault (cartChangeFromCartItem vr)
            |> Just
      }
    )
    (cartToCartItems oldCart)
    (cartToCartItems newCart)
    emptyCartDiff

calculateCartChangeEvent : RemoteCart -> Api.Cart -> List Effect
calculateCartChangeEvent remoteCart newCart =
  case getCart remoteCart of
    Just oldCart ->
      let
        cartDiff = diffCarts oldCart newCart
      in
        [ cartDiff.additions |> Maybe.map CartEvent.AddedToCart
        , cartDiff.removals  |> Maybe.map CartEvent.RemovedFromCart
        ]
          |> List.filterMap identity
          |> List.map Broadcast
    Nothing ->
      []

-- SUBSCRIPTIONS

subscriptions : Sub Msg
subscriptions =
  Decode.decodeValue messageDecoder
    >> Result.withDefault NoOpMsg
    |> messages

decodeTypeField : String -> Decoder Msg
decodeTypeField value =
  case value of

    "UpdateCart" ->
      Decode.map2
        UpdateCart
        (Decode.field "id" Decode.string)
        (Decode.field "quantity" Decode.int)

    "CheckoutCompleted" ->
      Decode.at ["order", "transactionId"] Decode.string
        |> Decode.map CheckoutCompleted

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
    addToCartButton = model.productVariantId
      |> Maybe.map buyButton
  in
    [ addToCartButton
    , Just <| cartButton (getCart model.cart)
    , Just cartContent
    ]
      |> List.filterMap identity
      |> H.div [ A.class "avenir", A.id "cart" ]


buyButton : String -> Html Msg
buyButton id =
  buttonPrimary
    [ A.class "mt2"
    , E.onClick (addToCart id)
    ]
    [ H.text "Add to cart"]

cartErrorView : Html Msg
cartErrorView =
  H.div [ A.class "ph3" ]
    [ H.p [] [ H.text "Something unexpected happened while trying to update your cart."]
    , buttonSecondarySmall
        [ E.onClick Retry ]
        [ H.text "Try Again" ]
    ]

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
          cartErrorView
        Updating cart change ->
          cartView cart
        Recreating cart change ->
          cartView cart
        RecreationFailed _ _ ->
          cartErrorView
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

cartFooter : Maybe Api.Cart -> Html Msg
cartFooter maybeCart =
  let
    content =
      case maybeCart of
        Just cart ->
          let
            checkoutButton = cart
              |> cartItemCount
              |> (\ count ->
                  if count > 0 then
                    buttonPrimary
                      [ A.class "db w-100"
                      , E.onClick checkout
                      ]
                      [ H.text "Checkout" ]
                  else
                    buttonPrimaryDisabled
                      [ A.class "db w-100" ]
                      [ H.text "Checkout" ]
                )
          in
            [ H.p [ A.class "" ]
                [ H.span [ A.class "" ] [ H.text "Subtotal:" ]
                , H.span [ A.class "fr fw6" ] [ H.text <| Money.toString cart.subTotal ]
                ]
            , H.p [ A.class "tc f6" ] [ H.text "Shipping and discount codes are added at checkout." ]
            , checkoutButton
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

lineView : Api.CartLine -> Html Msg
lineView { id, productVariant, quantity, subTotal } =
  let
    image = productVariant.image
      |> Maybe.map imageView
      |> Maybe.withDefault
          (H.div [ A.class "dib w3 h3 bg-light-gray" ] [])
    addButtonAttributes =
      if productVariant.quantityAvailable > quantity then
        [ A.class "ba br2 br--right pa2 dim pointer dib w2 tc b bg-white"
        , E.onClick (updateItemQuantityInCart productVariant.id (quantity + 1))
        ]
      else
        [ A.class "ba br2 br--right pa2 dib w2 tc b bg-white o-30"
        , A.disabled True
        ]
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
              , H.a addButtonAttributes [ H.text "+"]
              ]
          , H.span [ A.class "dib fr fw6 pv2" ] [ H.text <| Money.toString subTotal ]
          ]
      ]

-- VIEW COMPONENTS

button : List (Attribute msg) -> List (Html msg) -> Html msg
button attributes =
  let
    newAttributes =
      List.append
        [ A.class "f5 link mb2 br3" ]
        attributes
  in
    H.button newAttributes

buttonPrimary : List (Attribute msg) -> List (Html msg) -> Html msg
buttonPrimary attributes =
  attributes
    |> List.append [ A.class "dim pointer ph4 pv3 bg-black white bn" ]
    |> button

buttonPrimaryDisabled : List (Attribute msg) -> List (Html msg) -> Html msg
buttonPrimaryDisabled attributes =
  attributes
    |> List.append [ A.disabled True, A.class "ph4 pv3 bg-black white bn o-30" ]
    |> button

buttonSecondary : List (Attribute msg) -> List (Html msg) -> Html msg
buttonSecondary attributes =
  attributes
    |> List.append [ A.class "dim pointer ph4 pv3 bg-white black ba" ]
    |> button

buttonSecondarySmall : List (Attribute msg) -> List (Html msg) -> Html msg
buttonSecondarySmall attributes =
  attributes
    |> List.append [ A.class "dim pointer ph3 pv2 bg-white black ba" ]
    |> button

closeButton : Html Msg
closeButton =
  H.button
    [ A.class "f1 bn pointer dim ph0 bg-white"
    , E.onClick CloseCart
    ]
    -- TODO: Add aria attributes for screen readers
    [ H.text "×"]

cartButton : Maybe Api.Cart -> Html Msg
cartButton cart =
  let
    totalItemsText =
      cart
        |> Maybe.map cartItemCount
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
