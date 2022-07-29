-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Enum.DiscountApplicationTargetSelection exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The lines on the order to which the discount is applied, of the type defined by
the discount application's `targetType`. For example, the value `ENTITLED`, combined with a `targetType` of
`LINE_ITEM`, applies the discount on all line items that are entitled to the discount.
The value `ALL`, combined with a `targetType` of `SHIPPING_LINE`, applies the discount on all shipping lines.

  - All - The discount is allocated onto all the lines.
  - Entitled - The discount is allocated onto only the lines that it's entitled for.
  - Explicit - The discount is allocated onto explicitly chosen lines.

-}
type DiscountApplicationTargetSelection
    = All
    | Entitled
    | Explicit


list : List DiscountApplicationTargetSelection
list =
    [ All, Entitled, Explicit ]


decoder : Decoder DiscountApplicationTargetSelection
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ALL" ->
                        Decode.succeed All

                    "ENTITLED" ->
                        Decode.succeed Entitled

                    "EXPLICIT" ->
                        Decode.succeed Explicit

                    _ ->
                        Decode.fail ("Invalid DiscountApplicationTargetSelection type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : DiscountApplicationTargetSelection -> String
toString enum____ =
    case enum____ of
        All ->
            "ALL"

        Entitled ->
            "ENTITLED"

        Explicit ->
            "EXPLICIT"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe DiscountApplicationTargetSelection
fromString enumString____ =
    case enumString____ of
        "ALL" ->
            Just All

        "ENTITLED" ->
            Just Entitled

        "EXPLICIT" ->
            Just Explicit

        _ ->
            Nothing
