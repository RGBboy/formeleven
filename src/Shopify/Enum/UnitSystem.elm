-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Enum.UnitSystem exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| Systems of weights and measures.

  - ImperialSystem - Imperial system of weights and measures.
  - MetricSystem - Metric system of weights and measures.

-}
type UnitSystem
    = ImperialSystem
    | MetricSystem


list : List UnitSystem
list =
    [ ImperialSystem, MetricSystem ]


decoder : Decoder UnitSystem
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "IMPERIAL_SYSTEM" ->
                        Decode.succeed ImperialSystem

                    "METRIC_SYSTEM" ->
                        Decode.succeed MetricSystem

                    _ ->
                        Decode.fail ("Invalid UnitSystem type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : UnitSystem -> String
toString enum____ =
    case enum____ of
        ImperialSystem ->
            "IMPERIAL_SYSTEM"

        MetricSystem ->
            "METRIC_SYSTEM"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe UnitSystem
fromString enumString____ =
    case enumString____ of
        "IMPERIAL_SYSTEM" ->
            Just ImperialSystem

        "METRIC_SYSTEM" ->
            Just MetricSystem

        _ ->
            Nothing
