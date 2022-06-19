module Pages exposing (DataModel, generate)

import Components as C
import Html as H exposing (Attribute, Html)
import Html.Attributes as A

type alias DataModel =
  { products : List Product
  }

type alias Product =
  { id : String
  , title: String
  , description : String
  , media : List Image
  }

type alias Image =
  { alt: String
  , url : String -- Ideally this should be type URL
  }

generate : DataModel -> List (String, Html msg)
generate { products } =
  let
    productPages = List.map generateProductPage products
  in
    List.append
      [ ("/", home)
      , ("/products", productsPage products)
      , ("/terms", terms)
      ]
      productPages

productOverview : Product -> Html msg
productOverview product =
  let
    id = String.replace "gid://shopify/Product" "/products" product.id
    href = id ++ ".html" |> C.projectPath
  in
    H.div []
      [ H.a [ A.href href] [ H.text id ] ]

productsPage : List Product -> Html msg
productsPage products =
  let
    productList = List.map productOverview products
    pageTitle = C.h2 [] [ H.text "Products" ]
    content = pageTitle :: productList
  in
  C.layout
    [ H.section [] content ]

generateProductPage : Product -> (String, Html msg)
generateProductPage product =
  let
    id = String.replace "gid://shopify/Product" "/products" product.id
  in
    (id, productPage product)

productPage : Product -> Html msg
productPage product =
  let
    productImages = List.map productImage product.media
    productInfo =
      productTileInfo
        product.title
        product.description
        <| Nothing
    content = productInfo :: productImages
  in
    C.layout [ productLayout content ]

productImage : Image -> Html msg
productImage { url } =
  productTileImage url

productLayout : List (Html msg) -> Html msg
productLayout =
  H.div [ A.class "flex flex-wrap mv3" ]

productTile : List (Html msg) -> Html msg
productTile =
  productTileExtra []

productTileInfo : String -> String -> Maybe String -> Html msg
productTileInfo title description url =
  let
    link = Maybe.map (pillLink "Visit Store") url
    linkList = List.filterMap identity [ link ]
    content =
      List.append
        [ H.dl [ A.class "ma0 f6"]
            [ H.dt [ A.class "clip" ] [ H.text "Title"]
            , H.dd [ A.class "f3 ma0" ] [ H.text title ]
            , H.dt [ A.class "clip" ] [ H.text "Description"]
            , H.dd [ A.class "f5 fw2 ma0" ] [ H.text description ]
            ]
        ]
        linkList
  in
    productTileExtra [ A.class "tr-ns self-end-ns" ] content

productTileExtra : List (Attribute msg) -> List (Html msg) -> Html msg
productTileExtra attributes =
  A.class "w-100 w-third-m w-25-l pa2" :: attributes
    |> H.div

pillLink : String -> String -> Html msg
pillLink text href =
  H.a
    [ A.class "f6 grow no-underline br-pill ph3 pb1 pt2 mt2 dib white bg-black"
    , A.href href
    ]
    [ H.text text ]

productTileImage : String -> Html msg
productTileImage src =
  productTile
    [ H.img
      [ A.class "db"
      , A.src src
      ]
      []
    ]

productTileSpacerL : Html msg
productTileSpacerL =
  H.div [ A.class "w-0 w-25-l pa2-l db" ] []

productTileSpacerM : Html msg
productTileSpacerM =
  H.div [ A.class "w-0 w-third-m w-0-l pa2-m db" ] []

home : Html msg
home =
  C.layout
    [ H.section [ A.id "current-work" ]
        [ C.h2 [] [ H.text "Current Work" ]
        , productLayout
            [ productTileInfo
                "Ripple"
                "A digitally fabricated lamp shade made from recycled bioplastic. The form is based on an oscillating wave with a subtle distortion."
                <| Just "https://www.etsy.com/listing/1093803559/ripple-lamp-shade-e14-white-pendant"
            , productTileImage <| C.projectPath "/img/512x512/ripple-on-wood.jpg"
            , productTileImage <| C.projectPath "/img/512x512/ripple-close-up-on.jpg"
            , productTileImage <| C.projectPath "/img/512x512/ripple-in-situ-pendant.jpg"
            , productTileSpacerL
            , productTileImage <| C.projectPath "/img/512x512/ripple-pendant.jpg"
            , productTileImage <| C.projectPath "/img/512x512/ripple-in-situ.jpg"
            ]
        , productLayout
            [ productTileInfo
                "Finn"
                "A digitally fabricated lamp shade made from recycled bioplastic. The form is based on a cubic oscillation with a subtle distortion."
                <| Just "https://www.etsy.com/listing/1167534251/finn-lamp-shade-e27-white-pendant-light"
            , productTileImage <| C.projectPath "/img/512x512/finn-pendant.jpg"
            , productTileImage <| C.projectPath "/img/512x512/finn-close-up.jpg"
            , productTileSpacerM
            , productTileImage <| C.projectPath "/img/512x512/finn-in-situ.jpg"
            , productTileSpacerL
            , productTileImage <| C.projectPath "/img/512x512/finn-stand.jpg"
            ]
        , productLayout
            [ productTileInfo
                "Chimney Triplet"
                "Set of three small stoneware bud vases. Each a bold, simple shape, inspired by British, brutalist forms."
                <| Just "https://www.etsy.com/listing/1163010301/chimney-triplet-set-of-three-stoneware"
            , productTileImage <| C.projectPath "/img/512x512/chimney-triplet-004.jpg"
            , productTileImage <| C.projectPath "/img/512x512/chimney-triplet-002.jpg"
            , productTileSpacerM
            , productTileImage <| C.projectPath "/img/512x512/chimney-triplet-003.jpg"
            , productTileSpacerL
            , productTileImage <| C.projectPath "/img/512x512/chimney-triplet-001.jpg"
            ]
        ]
    , H.section [ A.id "prototypes" ]
        [ C.h2 [] [ H.text "Prototypes" ]
        , productLayout
            [ productTileInfo
                "Murmur"
                "Recycled&nbsp;Bioplastic, Ceramic, Light&nbsp;Fitting"
                <| Nothing
            , productTileImage <| C.projectPath "/img/512x512/murmur.jpg"
            , productTileImage <| C.projectPath "/img/512x512/murmur-close-up.jpg"
            ]
        , productLayout
            [ productTileInfo
                "Growth"
                "Digital"
                <| Nothing
            , productTileImage <| C.projectPath "/img/512x512/growth-vase-001-3.jpg"
            , productTileImage <| C.projectPath "/img/512x512/growth-vase-002-3.jpg"
            , productTileSpacerM
            , productTileImage <| C.projectPath "/img/512x512/growth-vase-003-3.jpg"
            ]
        , productLayout
            [ productTileInfo
                "Inflation"
                "Digital"
                <| Nothing
            , productTileImage <| C.projectPath "/img/512x512/inflation-001.jpg"
            , productTileImage <| C.projectPath "/img/512x512/inflation-002.jpg"
            , productTileSpacerM
            , productTileImage <| C.projectPath "/img/512x512/inflation-003.jpg"
            ]
        , productLayout
            [ productTileInfo
                "Seed"
                "Porcelain"
                <| Nothing
            , productTileImage <| C.projectPath "/img/512x512/seed.jpg"
            , productTileImage <| C.projectPath "/img/512x512/seed-triplet.jpg"
            ]
        , productLayout
            [ productTileInfo
                "Noise"
                "Digital"
                <| Nothing
            , productTileImage <| C.projectPath "/img/512x512/spiral-brick-cell-001.jpg"
            , productTileImage <| C.projectPath "/img/512x512/spiral-brick-voronoi-001.jpg"
            , productTileSpacerM
            , productTileImage <| C.projectPath "/img/512x512/spiral-distorted-noise-001.jpg"
            ]
        , productLayout
            [ productTileInfo
                "Epicycloid"
                "Digital"
                <| Nothing
            , productTileImage <| C.projectPath "/img/512x512/spirograph-lerp-3-5-0-16-top.jpg"
            , productTileImage <| C.projectPath "/img/512x512/spirograph-lerp-7-4-0-7-top.jpg"
            ]
        ]
    ]

-- At some point figure out how best to cater for large blocks of typography.
-- Perhaps dillonkearns/elm-markdown would be useful to achieve this.

terms : Html msg
terms =
  C.layout
    [ H.section
        [ A.class "mw6 center"
        , A.id "terms"
        ]
        [ C.h2 [] [ H.text "Terms & Conditions"]
        , C.h3 [ A.id "privacy"] [ H.text "Privacy Policy" ]
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
    ]
