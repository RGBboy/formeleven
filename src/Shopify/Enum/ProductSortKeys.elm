-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Enum.ProductSortKeys exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The set of valid sort keys for the Product query.

  - Title - Sort by the `title` value.
  - ProductType - Sort by the `product_type` value.
  - Vendor - Sort by the `vendor` value.
  - UpdatedAt - Sort by the `updated_at` value.
  - CreatedAt - Sort by the `created_at` value.
  - BestSelling - Sort by the `best_selling` value.
  - Price - Sort by the `price` value.
  - Id - Sort by the `id` value.
  - Relevance - Sort by relevance to the search terms when the `query` parameter is specified on the connection.
    Don't use this sort key when no search query is specified.

-}
type ProductSortKeys
    = Title
    | ProductType
    | Vendor
    | UpdatedAt
    | CreatedAt
    | BestSelling
    | Price
    | Id
    | Relevance


list : List ProductSortKeys
list =
    [ Title, ProductType, Vendor, UpdatedAt, CreatedAt, BestSelling, Price, Id, Relevance ]


decoder : Decoder ProductSortKeys
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "TITLE" ->
                        Decode.succeed Title

                    "PRODUCT_TYPE" ->
                        Decode.succeed ProductType

                    "VENDOR" ->
                        Decode.succeed Vendor

                    "UPDATED_AT" ->
                        Decode.succeed UpdatedAt

                    "CREATED_AT" ->
                        Decode.succeed CreatedAt

                    "BEST_SELLING" ->
                        Decode.succeed BestSelling

                    "PRICE" ->
                        Decode.succeed Price

                    "ID" ->
                        Decode.succeed Id

                    "RELEVANCE" ->
                        Decode.succeed Relevance

                    _ ->
                        Decode.fail ("Invalid ProductSortKeys type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ProductSortKeys -> String
toString enum____ =
    case enum____ of
        Title ->
            "TITLE"

        ProductType ->
            "PRODUCT_TYPE"

        Vendor ->
            "VENDOR"

        UpdatedAt ->
            "UPDATED_AT"

        CreatedAt ->
            "CREATED_AT"

        BestSelling ->
            "BEST_SELLING"

        Price ->
            "PRICE"

        Id ->
            "ID"

        Relevance ->
            "RELEVANCE"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ProductSortKeys
fromString enumString____ =
    case enumString____ of
        "TITLE" ->
            Just Title

        "PRODUCT_TYPE" ->
            Just ProductType

        "VENDOR" ->
            Just Vendor

        "UPDATED_AT" ->
            Just UpdatedAt

        "CREATED_AT" ->
            Just CreatedAt

        "BEST_SELLING" ->
            Just BestSelling

        "PRICE" ->
            Just Price

        "ID" ->
            Just Id

        "RELEVANCE" ->
            Just Relevance

        _ ->
            Nothing
