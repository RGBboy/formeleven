module ProductFeed exposing (ProductFeedData, generate)

import Components as C
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Json.Decode as Decode exposing (Decoder)


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
  , tags : List String
  , productHighlights : Maybe Metafield
  , productDetails : Maybe Metafield
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

type alias Metafield =
  { value : String
  }

-- this is duplicated in Pages.elm
decodeMetafield : Decoder a -> Metafield -> Maybe a
decodeMetafield decoder field =
  Decode.decodeString decoder field.value
    |> Result.toMaybe

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

-- This is shown to really impact what keywords you show up for. We use both
-- productType and tags as keywords here. Product type isn’t visible to
-- shoppers, only to Google.
itemProductType : String -> Html msg
itemProductType value =
  H.node "g:product_type" [] [ H.text value ]

-- product highlights are bullet points on your product detail pages. You can
-- include as many as 10 highlights per product. Google recommends four to six
-- highlights. Each highlight can be up to 150 characters.
itemProductHighlight : String -> Html msg
itemProductHighlight value =
  H.node "g:product_highlight" [] [ H.text value ]

-- this is duplicated in Pages.elm
productHighlightsDecoder : Decoder (List String)
productHighlightsDecoder =
  Decode.list Decode.string

generateProductHighlights : Maybe Metafield -> List (Html msg)
generateProductHighlights field =
  field
    |> Maybe.andThen (decodeMetafield productHighlightsDecoder)
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

-- this is duplicated in Pages.elm
decodeKeyValue : String -> Decoder (String, String)
decodeKeyValue value =
  case String.split ": " value of
    [k, v] -> Decode.succeed (k, v)
    _ -> Decode.fail "String does not contain a single \":\" delimeter"

-- this is duplicated in Pages.elm
productDetailsDecoder : Decoder (List (String, String))
productDetailsDecoder =
  Decode.string
    |> Decode.andThen decodeKeyValue
    |> Decode.list

generateProductDetails : Maybe Metafield -> List (Html msg)
generateProductDetails field =
  field
    |> Maybe.andThen (decodeMetafield productDetailsDecoder)
    |> Maybe.withDefault []
    |> List.map itemProductDetail

-- Feed Item
generateItem : Product -> Html msg
generateItem product =
  let
    images = nodeListList product.images
      |> generateImages
    -- we create this from Shopify > Product > ProductType
    -- and Shopify > Product > Tags
    productTypes =
      [ product.productType
      , String.join " | " product.tags
      ]
        |> List.map itemProductType
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
    items = nodeListList products
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
