-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Enum.ProductMediaSortKeys exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The set of valid sort keys for the ProductMedia query.

  - Position - Sort by the `position` value.
  - Id - Sort by the `id` value.
  - Relevance - Sort by relevance to the search terms when the `query` parameter is specified on the connection.
    Don't use this sort key when no search query is specified.

-}
type ProductMediaSortKeys
    = Position
    | Id
    | Relevance


list : List ProductMediaSortKeys
list =
    [ Position, Id, Relevance ]


decoder : Decoder ProductMediaSortKeys
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "POSITION" ->
                        Decode.succeed Position

                    "ID" ->
                        Decode.succeed Id

                    "RELEVANCE" ->
                        Decode.succeed Relevance

                    _ ->
                        Decode.fail ("Invalid ProductMediaSortKeys type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ProductMediaSortKeys -> String
toString enum____ =
    case enum____ of
        Position ->
            "POSITION"

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
fromString : String -> Maybe ProductMediaSortKeys
fromString enumString____ =
    case enumString____ of
        "POSITION" ->
            Just Position

        "ID" ->
            Just Id

        "RELEVANCE" ->
            Just Relevance

        _ ->
            Nothing
