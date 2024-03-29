-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Enum.ProductCollectionSortKeys exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The set of valid sort keys for the ProductCollection query.

  - Title - Sort by the `title` value.
  - Price - Sort by the `price` value.
  - BestSelling - Sort by the `best-selling` value.
  - Created - Sort by the `created` value.
  - Id - Sort by the `id` value.
  - Manual - Sort by the `manual` value.
  - CollectionDefault - Sort by the `collection-default` value.
  - Relevance - Sort by relevance to the search terms when the `query` parameter is specified on the connection.
    Don't use this sort key when no search query is specified.

-}
type ProductCollectionSortKeys
    = Title
    | Price
    | BestSelling
    | Created
    | Id
    | Manual
    | CollectionDefault
    | Relevance


list : List ProductCollectionSortKeys
list =
    [ Title, Price, BestSelling, Created, Id, Manual, CollectionDefault, Relevance ]


decoder : Decoder ProductCollectionSortKeys
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "TITLE" ->
                        Decode.succeed Title

                    "PRICE" ->
                        Decode.succeed Price

                    "BEST_SELLING" ->
                        Decode.succeed BestSelling

                    "CREATED" ->
                        Decode.succeed Created

                    "ID" ->
                        Decode.succeed Id

                    "MANUAL" ->
                        Decode.succeed Manual

                    "COLLECTION_DEFAULT" ->
                        Decode.succeed CollectionDefault

                    "RELEVANCE" ->
                        Decode.succeed Relevance

                    _ ->
                        Decode.fail ("Invalid ProductCollectionSortKeys type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ProductCollectionSortKeys -> String
toString enum____ =
    case enum____ of
        Title ->
            "TITLE"

        Price ->
            "PRICE"

        BestSelling ->
            "BEST_SELLING"

        Created ->
            "CREATED"

        Id ->
            "ID"

        Manual ->
            "MANUAL"

        CollectionDefault ->
            "COLLECTION_DEFAULT"

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
fromString : String -> Maybe ProductCollectionSortKeys
fromString enumString____ =
    case enumString____ of
        "TITLE" ->
            Just Title

        "PRICE" ->
            Just Price

        "BEST_SELLING" ->
            Just BestSelling

        "CREATED" ->
            Just Created

        "ID" ->
            Just Id

        "MANUAL" ->
            Just Manual

        "COLLECTION_DEFAULT" ->
            Just CollectionDefault

        "RELEVANCE" ->
            Just Relevance

        _ ->
            Nothing
