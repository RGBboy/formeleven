module ProductFeed exposing (ProductFeedData, generate)

import Components as C
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Json.Decode as Decode exposing (Decoder)
import ProductData


type alias ProductFeedData =
  { products : ProductData.NodeList Product
  }

type alias Product =
  { id : String
  , handle : String
  , title: String
  , description : String
  , productType : String
  , availableForSale : Bool
  , totalInventory : Int
  , priceRange : { maxVariantPrice : ProductData.Money }
  , images : ProductData.NodeList ProductData.Image
  , tags : List String
  , productHighlights : Maybe ProductData.Metafield
  , productDetails : Maybe ProductData.Metafield
  }

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

itemPrice : ProductData.Money -> Html msg
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

itemImageLink : ProductData.Image -> Html msg
itemImageLink image =
  H.node "g:image_link" [] [ H.text image.url ]

itemAdditionalImageLink : ProductData.Image -> Html msg
itemAdditionalImageLink image =
  H.node "g:additional_image_link" [] [ H.text image.url ]

generateImages : List ProductData.Image -> List (Html msg)
generateImages images =
  case images of
    x :: [] -> [ itemImageLink x ]
    x :: xs -> itemImageLink x :: List.map itemAdditionalImageLink xs
    [] -> []

-- This is shown to really impact what keywords you show up for. We use both
-- productType and tags as keywords here. Product type isn’t visible to
-- shoppers, only to Google.
itemProductType : String -> Html msg
itemProductType value =
  H.node "g:product_type" [] [ H.text value ]

-- we create this from Shopify > Product > ProductType
-- and Shopify > Product > Tags
generateProductTypes : String -> List String -> List (Html msg)
generateProductTypes productType tags =
  [ productType
  , String.join " | " tags
  ]
    |> List.filter (not << String.isEmpty)
    |> List.map itemProductType

-- product highlights are bullet points on your product detail pages. You can
-- include as many as 10 highlights per product. Google recommends four to six
-- highlights. Each highlight can be up to 150 characters.
itemProductHighlight : String -> Html msg
itemProductHighlight value =
  H.node "g:product_highlight" [] [ H.text value ]

generateProductHighlights : Maybe ProductData.Metafield -> List (Html msg)
generateProductHighlights field =
  field
    |> Maybe.andThen (ProductData.decodeMetafield ProductData.productHighlightsDecoder)
    |> Maybe.withDefault []
    |> List.map itemProductHighlight

-- Additional product information e.g. technical specifications like product
-- dimensions, materials etc. if those are important bits of information and
-- not covered elsewhere in your feed.
itemProductDetail : (String, String) -> Html msg
itemProductDetail (key, value) =
  H.node "g:product_detail" []
    [ H.node "g:attribute_name" [] [ H.text key ]
    , H.node "g:attribute_value" [] [ H.text value ]
    ]

generateProductDetails : Maybe ProductData.Metafield -> List (Html msg)
generateProductDetails field =
  field
    |> Maybe.andThen (ProductData.decodeMetafield ProductData.productDetailsDecoder)
    |> Maybe.withDefault []
    |> List.map itemProductDetail

-- Feed Item
generateItem : Product -> Html msg
generateItem product =
  let
    images = ProductData.nodeListList product.images
      |> generateImages
    productTypes = generateProductTypes product.productType product.tags
    highlights = product.productHighlights |> generateProductHighlights
    productDetails = product.productDetails |> generateProductDetails
  in
    H.node "item" []
      <| List.concat
          [ [ itemId product.id
            , H.node "g:title" [] [ H.text product.title ]
            , H.node "g:description" [] [ H.text product.description ]
            , itemLink product
            , H.node "g:brand" [] [ H.text "Form Eleven" ]
            , H.node "g:identifier_exists" [] [ H.text "no" ]
            , H.node "g:condition" [] [ H.text "new" ]
            , itemAvailability product.availableForSale
            , H.node "g:quantity_to_sell_on_facebook" [] [ H.text <| String.fromInt product.totalInventory ]
            , itemPrice product.priceRange.maxVariantPrice
            -- this will need to be pulled from Shopify eventually
            , H.node "g:shipping" []
              [ H.node "g:country" [] [ H.text "UK" ]
              , H.node "g:service" [] [ H.text "Standard" ]
              , H.node "g:price" [] [ H.text "0.00 GBP" ]
              ]
            , itemGoogleProductCategory product.productType
            ]
          , images
          , productTypes
          , highlights
          , productDetails
          ]

-- At some point this should really return a Result X (Html msg)
-- so that any errors that come from decoding and transforming the data
-- can be caught and handled
generate : ProductFeedData -> Html msg
generate { products } =
  let
    items = ProductData.nodeListList products
      |> List.map generateItem
  in
    H.node "rss"
      [ A.attribute "xmlns:g" "http://base.google.com/ns/1.0"
      , A.attribute "version" "2.0"
      ]
      [ H.node "channel" []
        <| List.append
            [ H.node "title" [] [ H.text "Form Eleven Store" ]
            ]
            items
      ]
