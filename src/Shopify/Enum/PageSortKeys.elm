-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Enum.PageSortKeys exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The set of valid sort keys for the Page query.

  - Title - Sort by the `title` value.
  - UpdatedAt - Sort by the `updated_at` value.
  - Id - Sort by the `id` value.
  - Relevance - Sort by relevance to the search terms when the `query` parameter is specified on the connection.
    Don't use this sort key when no search query is specified.

-}
type PageSortKeys
    = Title
    | UpdatedAt
    | Id
    | Relevance


list : List PageSortKeys
list =
    [ Title, UpdatedAt, Id, Relevance ]


decoder : Decoder PageSortKeys
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "TITLE" ->
                        Decode.succeed Title

                    "UPDATED_AT" ->
                        Decode.succeed UpdatedAt

                    "ID" ->
                        Decode.succeed Id

                    "RELEVANCE" ->
                        Decode.succeed Relevance

                    _ ->
                        Decode.fail ("Invalid PageSortKeys type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : PageSortKeys -> String
toString enum____ =
    case enum____ of
        Title ->
            "TITLE"

        UpdatedAt ->
            "UPDATED_AT"

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
fromString : String -> Maybe PageSortKeys
fromString enumString____ =
    case enumString____ of
        "TITLE" ->
            Just Title

        "UPDATED_AT" ->
            Just UpdatedAt

        "ID" ->
            Just Id

        "RELEVANCE" ->
            Just Relevance

        _ ->
            Nothing
