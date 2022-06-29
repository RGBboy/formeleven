module ProductFeed exposing (ProductFeedData, generate)

import Components as C
import Html as H exposing (Attribute, Html)
import Html.Attributes as A



type alias ProductFeedData =
  { products : NodeList Product
  }

type alias Product =
  { id : String
  , handle : String
  , title: String
  , description : String
  , productType : String
  , availableForSale : Bool
  , priceRange : { maxVariantPrice : Money }
  , images : NodeList Image
  }

-- Todo: Change this type to an actual monetary type.
-- Perhaps move to use a JSON.Value for flags then decode to
-- correctly handle input data and errors
type alias Money =
  { amount: String
  , currencyCode : String
  }

type alias Image =
  { url : String -- Ideally this should be type URL
  }

type alias NodeList a =
  { nodes: List a
  }

nodeListList : NodeList a -> List a
nodeListList { nodes } = nodes

generate : ProductFeedData -> Html msg
generate { products } =
  let
    items = nodeListList products
      |> List.map generateItem
  in
    H.node "channel" []
      <| List.append
          [ H.node "title" [] [ H.text "Form Eleven Store" ]
          -- , H.node "link" [] [ H.text "test link" ]
          -- , H.node "description" [] [ H.text "test desciption" ]
          ]
          items

itemId : String -> Html msg
itemId id =
  let
    -- move this into Shopify module
    value = String.replace "gid://shopify/Product/" "" id
  in
    H.node "g:id" [] [ H.text value ]

itemAvailability : Bool -> Html msg
itemAvailability availableForSale =
  let
    value =
      case availableForSale of
        True -> "in_stock"
        False -> "out_of_stock"
  in
    H.node "g:availability" [] [ H.text value ]

itemPrice : Money -> Html msg
itemPrice { amount, currencyCode } =
  let
    value = amount ++ " " ++ currencyCode
  in
    H.node "g:price" [] [ H.text value ]

itemLink : Product -> Html msg
itemLink product =
  let
    value = "https://www.formeleven.com/products/" ++ product.handle ++ ".html"
  in
    H.node "g:link" [] [ H.text value ]

itemGoogleProductCategory : String -> Html msg
itemGoogleProductCategory productType =
  let
    value =
      case productType of
        "Lamp Shades" ->
          "Home &amp; Garden &gt; Lighting Accessories &gt; Lampshades"
        "Light Sculptures" ->
          "Home &amp; Garden &gt; Decor &gt; Artwork &gt; Sculptures &amp; Statues"
        _ ->
          "Home &amp; Garden &gt; Decor &gt; Artwork"
  in
    H.node "g:google_product_category" [] [ H.text value ]

itemImageLink : Image -> Html msg
itemImageLink image =
  H.node "g:image_link" [] [ H.text image.url ]

itemAdditionalImageLink : Image -> Html msg
itemAdditionalImageLink image =
  H.node "g:additional_image_link" [] [ H.text image.url ]

generateImages : List Image -> List (Html msg)
generateImages images =
  case images of
    x :: [] -> [ itemImageLink x ]
    x :: xs -> itemImageLink x :: List.map itemAdditionalImageLink xs
    [] -> []

-- Feed Item

generateItem : Product -> Html msg
generateItem product =
  let
    images = generateImages <| nodeListList product.images
  in
    H.node "item" []
      <| List.append
          [ itemId product.id
          , H.node "g:title" [] [ H.text product.title ]
          , H.node "g:description" [] [ H.text product.description ]
          , itemLink product
          , H.node "g:identifier_exists" [] [ H.text "no" ]
          , H.node "g:condition" [] [ H.text "new" ]
          , itemAvailability product.availableForSale
          , itemPrice product.priceRange.maxVariantPrice
          -- this will need to be pulled from Shopify eventually
          , H.node "g:shipping" []
            [ H.node "g:country" [] [ H.text "UK" ]
            , H.node "g:service" [] [ H.text "Standard" ]
            , H.node "g:price" [] [ H.text "0.00 GBP" ]
            ]
          , itemGoogleProductCategory product.productType
          ]
          images
