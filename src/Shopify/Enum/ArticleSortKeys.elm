-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Enum.ArticleSortKeys exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The set of valid sort keys for the Article query.

  - Title - Sort by the `title` value.
  - BlogTitle - Sort by the `blog_title` value.
  - Author - Sort by the `author` value.
  - UpdatedAt - Sort by the `updated_at` value.
  - PublishedAt - Sort by the `published_at` value.
  - Id - Sort by the `id` value.
  - Relevance - Sort by relevance to the search terms when the `query` parameter is specified on the connection.
    Don't use this sort key when no search query is specified.

-}
type ArticleSortKeys
    = Title
    | BlogTitle
    | Author
    | UpdatedAt
    | PublishedAt
    | Id
    | Relevance


list : List ArticleSortKeys
list =
    [ Title, BlogTitle, Author, UpdatedAt, PublishedAt, Id, Relevance ]


decoder : Decoder ArticleSortKeys
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "TITLE" ->
                        Decode.succeed Title

                    "BLOG_TITLE" ->
                        Decode.succeed BlogTitle

                    "AUTHOR" ->
                        Decode.succeed Author

                    "UPDATED_AT" ->
                        Decode.succeed UpdatedAt

                    "PUBLISHED_AT" ->
                        Decode.succeed PublishedAt

                    "ID" ->
                        Decode.succeed Id

                    "RELEVANCE" ->
                        Decode.succeed Relevance

                    _ ->
                        Decode.fail ("Invalid ArticleSortKeys type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : ArticleSortKeys -> String
toString enum____ =
    case enum____ of
        Title ->
            "TITLE"

        BlogTitle ->
            "BLOG_TITLE"

        Author ->
            "AUTHOR"

        UpdatedAt ->
            "UPDATED_AT"

        PublishedAt ->
            "PUBLISHED_AT"

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
fromString : String -> Maybe ArticleSortKeys
fromString enumString____ =
    case enumString____ of
        "TITLE" ->
            Just Title

        "BLOG_TITLE" ->
            Just BlogTitle

        "AUTHOR" ->
            Just Author

        "UPDATED_AT" ->
            Just UpdatedAt

        "PUBLISHED_AT" ->
            Just PublishedAt

        "ID" ->
            Just Id

        "RELEVANCE" ->
            Just Relevance

        _ ->
            Nothing
