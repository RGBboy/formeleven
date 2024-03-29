module Pages exposing (DataModel, Page, generate)

import Components as C
import Html as H exposing (Attribute, Html)
import Html.Attributes as A
import Html.Parser
import Html.Parser.Util
import Json.Decode as Decode exposing (Decoder)
import Maybe.Extra
import ProductData
import ProductFeed exposing (ProductFeedData)
import SiteMap


type alias DataModel =
  { collection : ProductCollection
  , feed : ProductFeedData
  , shop : ShopData
  }

type alias ProductCollection =
  { id : String
  , handle : String
  , title : String
  , descriptionHtml : String
  , products : ProductData.NodeList Product
  }

type alias ProductVariant =
  { id : String }

type alias Product =
  { id : String
  , handle : String
  , variants : ProductData.NodeList ProductVariant
  , title : String
  , description : String
  , descriptionHtml : String
  , availableForSale : Bool
  , priceRange : { maxVariantPrice : ProductData.Money }
  , featuredImage : ProductData.Image
  , images : ProductData.NodeList ProductData.Image
  , productHighlights : Maybe ProductData.Metafield
  , productMetadata : Maybe ProductData.Metafield
  }

type alias ShopData =
  { refundPolicy : { body : String }
  }

type alias Page msg =
  { title : String
  , description : String
  , body : Html msg
  }

page : String -> String -> Html msg -> Page msg
page title description body =
  { title = title
  , description = description
  , body = body
  }

generate : DataModel -> List (String, Page msg)
generate { collection, feed, shop } =
  let
    productPages = ProductData.nodeListList collection.products
      |> List.map generateProductPage
    allPages =
      List.append
        [ ("/index.html"
          , page "Form Eleven" "Combining digital fabrication, handmade ceramics and luminaires." (homePage collection)
          )
        , ("/terms.html"
          , page "Terms & Conditions" "Terms & Conditions" (termsPage shop.refundPolicy.body)
          )
        , ("/product-feed.xml"
          , page "" "" (ProductFeed.generate feed)
          )
        ]
        productPages
    siteMap =
      List.map Tuple.first allPages
        |> List.filter isHtml
        |> SiteMap.generate
  in
    ("/sitemap.xml", page "" "" siteMap) :: allPages

isHtml : String -> Bool
isHtml file =
  case (String.right 5 file) of
    ".html" -> True
    _ -> False

localProductIdFromGlobalProductId : String -> String
localProductIdFromGlobalProductId globalId =
  String.replace "gid://shopify/Product/" "" globalId

localProductVariantIdFromGlobalProductVariantId : String -> String
localProductVariantIdFromGlobalProductVariantId globalId =
  String.replace "gid://shopify/ProductVariant/" "" globalId

productPathFromHandle : String -> String
productPathFromHandle handle =
  "/products/" ++ handle ++ ".html"

findMaxIndex : Int -> Int -> Int -> Int
findMaxIndex max value acc =
  if value > max then acc else value

metaDescription : String -> String
metaDescription description =
  let
    indices = String.indices "." description
    maxValue = 160
    index = List.foldl (findMaxIndex maxValue) 0 indices
  in
    String.slice 0 (index + 1) description

-- Product Page

generateProductPage : Product -> (String, Page msg)
generateProductPage product =
  ( productPathFromHandle product.handle
  , page product.title (metaDescription product.description) (productPage product)
  )

boolToMaybe : Bool -> Maybe ()
boolToMaybe value =
  case value of
    True -> Just ()
    False -> Nothing

productPage : Product -> Html msg
productPage product =
  let
    availableForSale = boolToMaybe product.availableForSale
    buyButton = product.variants
      |> ProductData.nodeListList
      |> List.map .id
      |> List.map localProductVariantIdFromGlobalProductVariantId
      |> List.head
      |> Maybe.Extra.next availableForSale
      |> Maybe.map C.buyButton
      |> Maybe.withDefault
          ( H.div [ A.class "mv2" ]
              [ C.soldOut
              , C.cart
              ]
          )
    price = product.priceRange.maxVariantPrice.amount
    titlePrice =
      [ H.h1 [ A.class "f3 fw4 mb2 mt4 mt2-ns measure" ] [ H.text product.title ]
      , H.p [ A.class "f3 fw2 mv2 measure" ] [ H.text <| C.formatGBP price ]
      , H.span [ A.class "db fw4" ] [ H.text "Free UK delivery" ]
      ]
    highlights = product.productHighlights |> generateProductHighlights
    productDetails = product.productMetadata |> generateProductDetailsFromMetadata
    -- At some point we will need to parse product.descriptionHtml for safety
    content =
      [ H.text product.descriptionHtml
      , highlights
      , productDetails
      , H.p [] [ H.text "Made by Form Eleven in London, UK." ]
      , H.p [] [ H.text """
We care about our impact on the planet, so we ship our products with 100%
recyclable packaging and labels.
"""
          ]
      ]
  in
    C.layout
      [ H.div [ A.class "ph2" ] [ C.backButton ]
      , H.div [ A.class "db dn-ns ph2" ] titlePrice
      , H.div [ A.class "flex mv3 flex-wrap" ]
          [ H.div [ A.class "w-100 w-50-ns ph1" ]
              [ gallery <| ProductData.nodeListList product.images ]
          , H.div [ A.class "w-100 w-50-ns ph2" ]
              [ H.div [ A.class "dn db-ns" ] titlePrice
              , buyButton
              , H.div [ A.class "f4 fw2 measure" ] content
              ]
          ]
      ]

-- product highlights are bullet points on your product detail pages. You can
-- include as many as 10 highlights per product. Google recommends four to six
-- highlights. Each highlight can be up to 150 characters.
itemProductHighlight : String -> Html msg
itemProductHighlight value =
  H.li [ A.class "" ] [ H.text value ]

generateProductHighlights : Maybe ProductData.Metafield -> Html msg
generateProductHighlights field =
  field
    |> Maybe.andThen (ProductData.decodeMetafield ProductData.productHighlightsDecoder)
    |> Maybe.withDefault []
    |> List.map itemProductHighlight
    |> H.ul [ A.class "" ]

productDetailItem : (String, String) -> Html msg
productDetailItem (key, value) =
  H.li [ A.class "f4 mv2"]
    [ H.b [ A.class "fw4" ] [ H.text (key ++ ":") ]
    , H.text (" " ++ value)
    ]

generateProductDetailsFromMetadata : Maybe ProductData.Metafield -> Html msg
generateProductDetailsFromMetadata field =
  field
    |> Maybe.andThen (ProductData.decodeMetafield ProductData.productMetadataDecoder)
    |> Maybe.map ProductData.productDetailsMetadataKeyValues
    |> Maybe.withDefault []
    |> List.map productDetailItem
    |> H.ul [ A.class "list pl0" ]

-- contains IDs, may want to update to pass in a prefix to allow multiple on a page
gallery : List ProductData.Image -> Html msg
gallery images =
  H.div
    [ A.class "flex flex-wrap mb3" ]
    <| List.concat
    <| List.indexedMap galleryItem images

galleryItem : Int -> ProductData.Image -> List (Html msg)
galleryItem index { url } =
  let
    checked =
      case index of
        0 -> [ A.checked True ]
        _ -> []
    stringIndex = String.fromInt index
    id = "gallery--item__" ++ stringIndex
    inputAttributes =
      List.append
        [ A.id id
        , A.class "gallery--item--control"
        , A.type_ "radio"
        , A.name "gallery"
        , A.value stringIndex
        ]
        checked
  in
    [ H.input inputAttributes []
    , H.label
        [ A.for id
        , A.class "gallery--item--thumbnail"
        , A.class "order-1 dim w-20"
        ]
        [ H.span [ A.class "db pv1 ph1" ]
            [ H.img
                [ A.class "db mw-100 h-auto bg-light-gray"
                , A.src url
                , A.width 512
                , A.height 512
                ]
                []
            ]
        ]
    , H.div
        [ A.class "gallery--item--image"
        , A.class "order-0 ph1 pb1"
        ]
        [ H.img
            [ A.class "db mw-100 h-auto bg-light-gray"
            , A.src url
            , A.width 512
            , A.height 512
            ]
            []
        ]
    ]

-- Home Page

productListView : List Product -> Html msg
productListView products =
  let
    content = List.map productListItem products
  in
    H.div [ A.class "flex flex-wrap mv3" ] content

productListItem : Product -> Html msg
productListItem product =
  let
    localId = localProductIdFromGlobalProductId product.id
    price = product.priceRange.maxVariantPrice.amount
    productPath = productPathFromHandle product.handle
    href = "/frontend" ++ productPath
    url = product.featuredImage.url
    soldOut =
      case product.availableForSale of
        True ->
          []
        False ->
          [ H.div [ A.class "absolute top-1 right-1"]
              [ C.soldOut]
          ]
  in
    H.a
      [ A.class "link black dim relative w-100 w-third-m w-25-l pa2"
      , A.href href
      ]
      <| List.append soldOut
        [ H.div
            [ A.class "br2 ba b--black-10 w-100 flex flex-column-reverse" ]
            [ H.div [ A.class "self-start-ns pa2" ]
                [ H.h2
                    [ A.class "f4 f5-ns fw4 mv1" ]
                    [ H.text product.title ]
                , H.p
                    [ A.class "f4 f5-ns fw2 mv1" ]
                    [ H.text <| C.formatGBP price ]
                ]
            , H.img
                [ A.class "db mw-100 h-auto bg-light-gray"
                , A.src url
                , A.width 512
                , A.height 512
                ]
                []
            ]
        ]

homePage : ProductCollection -> Html msg
homePage collection =
  C.layout
    [ C.section [ A.id "shop" ]
        [ C.h2 [] [ H.text "Shop" ]
        , productListView <| ProductData.nodeListList collection.products
        , C.cart
        ]
    , C.section [ A.id "prototypes" ]
        [ C.h2 [] [ H.text "Prototypes" ]
        , C.tileLayout
            [ C.tileFirst
                [ C.tileInfo
                  "Murmur"
                  "Recycled&nbsp;Bioplastic, Ceramic, Light&nbsp;Fitting"
                ]
            , C.tileImage "/frontend/img/512x512/murmur.jpg"
            , C.tileImage "/frontend/img/512x512/murmur-close-up.jpg"
            ]
        , C.tileLayout
            [ C.tileFirst
                [ C.tileInfo
                    "Chimney Triplet"
                    "Set of three small stoneware bud vases. Each a bold, simple shape, inspired by British, brutalist forms."
                ]
            , C.tileImage "/frontend/img/512x512/chimney-triplet-004.jpg"
            , C.tileImage "/frontend/img/512x512/chimney-triplet-002.jpg"
            , C.tileSpacerM
            , C.tileImage "/frontend/img/512x512/chimney-triplet-003.jpg"
            , C.tileSpacerL
            , C.tileImage "/frontend/img/512x512/chimney-triplet-001.jpg"
            ]
        , C.tileLayout
            [ C.tileFirst
                [ C.tileInfo
                    "Growth"
                    "Digital"
                ]
            , C.tileImage "/frontend/img/512x512/growth-vase-001-3.jpg"
            , C.tileImage "/frontend/img/512x512/growth-vase-002-3.jpg"
            , C.tileSpacerM
            , C.tileImage "/frontend/img/512x512/growth-vase-003-3.jpg"
            ]
        , C.tileLayout
            [ C.tileFirst
                [ C.tileInfo
                    "Inflation"
                    "Digital"
                ]
            , C.tileImage "/frontend/img/512x512/inflation-001.jpg"
            , C.tileImage "/frontend/img/512x512/inflation-002.jpg"
            , C.tileSpacerM
            , C.tileImage "/frontend/img/512x512/inflation-003.jpg"
            ]
        , C.tileLayout
            [ C.tileFirst
                [ C.tileInfo
                    "Seed"
                    "Porcelain"
                ]
            , C.tileImage "/frontend/img/512x512/seed.jpg"
            , C.tileImage "/frontend/img/512x512/seed-triplet.jpg"
            ]
        , C.tileLayout
            [ C.tileFirst
                [ C.tileInfo
                    "Noise"
                    "Digital"
                ]
            , C.tileImage "/frontend/img/512x512/spiral-brick-cell-001.jpg"
            , C.tileImage "/frontend/img/512x512/spiral-brick-voronoi-001.jpg"
            , C.tileSpacerM
            , C.tileImage "/frontend/img/512x512/spiral-distorted-noise-001.jpg"
            ]
        , C.tileLayout
            [ C.tileFirst
                [ C.tileInfo
                    "Epicycloid"
                    "Digital"
                ]
            , C.tileImage "/frontend/img/512x512/spirograph-lerp-3-5-0-16-top.jpg"
            , C.tileImage "/frontend/img/512x512/spirograph-lerp-7-4-0-7-top.jpg"
            ]
        ]
    ]

-- Terms Page

-- At some point figure out how best to cater for large blocks of typography.
-- Perhaps dillonkearns/elm-markdown would be useful to achieve this.
termsPage : String -> Html msg
termsPage unparsedRefundPolicy =
  let
    parsedRefundPolicy =
      Html.Parser.run unparsedRefundPolicy
        |> Result.withDefault []
        |> Html.Parser.Util.toVirtualDom
    refundPolicy =
      List.append
        [ C.h3 [ A.id "refund"] [ H.text "Return & Refund Policy" ] ]
        parsedRefundPolicy
    privacyPolicy =
      [ C.h3 [ A.id "privacy"] [ H.text "Privacy Policy" ]
      , C.p """
This page is used to inform website visitors regarding our policies with the
collection, use, and disclosure of Personal Information.
"""
      , C.h4 [] [ H.text "Information I Collect" ]
      , C.p """
To subscribe to my mailing list you must provide me with certain information
such as your name and email address.
"""
      , C.p """
To fulfil your order, you must provide me with certain information (which you
authorised Etsy to provide to me), such as your name, email address,
postal address, payment information, and the details of the product that you’re
ordering. You may also choose to provide me with additional personal
information (for a custom order for example), if you contact me directly.
"""
      , C.h4 [] [ H.text "Why I Need Your Information and How I Use It" ]
      , C.p """
I rely on a number of legal bases to collect, use, and share your information,
including:
"""
      , C.ul
          [ C.li """
as needed to provide my services, such as when I use your information to fulfil
your order, to settle disputes, or to provide customer support;
"""
          , C.li """
when you have provided your affirmative consent, which you may revoke at any
time, such as by subscribing to my mailing list;
"""
          , C.li """
if necessary to comply with a legal obligation or court order or in connection
with a legal claim, such as retaining information about your purchases if
required by tax law; and
"""
          , C.li """
as necessary for the purpose of my legitimate interests, if those legitimate
interests are not overridden by your rights or interests, such as 1)
providing and improving my services. I use your information to provide the
services you requested and in my legitimate interest to improve my services;
and 2) Compliance with the Etsy Seller Policy and Terms of Use. I use your
information as necessary to comply with my obligations under the Etsy Seller
Policy and Terms of Use.
"""
          ]
      , C.h4 [] [ H.text "Information Sharing and Disclosure" ]
      , C.p """
Information about my customers is important to my business. I share your
personal information for very limited reasons and in limited circumstances,
as follows:
"""
      , C.ul
          [ C.li """
Etsy. I share information with Etsy as necessary to provide you my services and
comply with my obligations under both the Etsy Seller Policy and Etsy Terms of
Use.
"""
          , C.li """
Service providers. I engage certain trusted third parties to perform functions
and provide services to my shop, such as delivery companies. I will share your
personal information with these third parties, but only to the extent necessary
to perform these services.
"""
          , C.li """
Business transfers. If I sell or merge my business, I may disclose your
information as part of that transaction, only to the extent permitted by law.
"""
          , C.li """
Compliance with laws. I may collect, use, retain, and share your information if
I have a good faith belief that it is reasonably necessary to: (a) respond to
legal process or to government requests; (b) enforce my agreements, terms and
policies; (c) prevent, investigate, and address fraud and other illegal
activity, security, or technical issues; or (d) protect the rights, property,
and safety of my customers, or others.
"""
          ]
      , C.h4 [] [ H.text "Data Retention" ]
      , C.p """
I retain your personal information only for as long as necessary to provide you
with my services and as described in my Privacy Policy. However, I may also be
required to retain this information to comply with my legal and regulatory
obligations, to resolve disputes, and to enforce my agreements. I generally
keep your data for the following time period: 4 years.
"""
      , C.h4 [] [ H.text "Transfers of Personal Information Outside the EU" ]
      , C.p """
I may store and process your information through third-party hosting services
in the US and other jurisdictions. As a result, I may transfer your personal
information to a jurisdiction with different data protection and government
surveillance laws than your jurisdiction. If I am deemed to transfer
information about you outside of the EU, I rely on Privacy Shield as the legal
basis for the transfer, as Google Cloud is Privacy Shield certified.
"""
      , C.h4 [] [ H.text "Your Rights" ]
      , C.p """
If you reside in certain territories, including the EU, you have a number of
rights in relation to your personal information. While some of these rights
apply generally, certain rights apply only in certain limited cases. I
describe these rights below:
"""
      , C.ul
          [ C.li """
Access. You may have the right to access and receive a copy of the personal
information I hold about you by contacting me using the contact details
provided by Etsy or via <a class="link black underline dim"
href="mailto:hello@formeleven.com">hello@formeleven.com</a>.
"""
          , C.li """
Change, restrict, delete. You may also have rights to change, restrict my use
of, or delete your personal information. Absent exceptional circumstances (like
where I am required to store data for legal reasons) I will generally delete
your personal information upon request.
"""
          , C.li """
Object. You can object to (i) my processing of some of your information based
on my legitimate interests and (ii) receiving marketing messages from me after
providing your express consent to receive them. In such cases, I will delete
your personal information unless I have compelling and legitimate grounds to
continue using that information or if it is needed for legal reasons.
"""
          , C.li """
Complain. If you reside in the EU and wish to raise a concern about my use of
your information (and without prejudice to any other rights you may have), you
have the right to do so with your local data protection authority.
"""
          ]
      ]
    body =
      List.concat
        [ [ C.h2 [] [ H.text "Terms & Conditions"] ]
        , refundPolicy
        , privacyPolicy
        ]
  in
  C.layout
    [ C.section
        [ A.class "mw6 center"
        , A.id "terms"
        ]
        body
    ]
