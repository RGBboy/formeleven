-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Enum.UnitPriceMeasurementMeasuredUnit exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The valid units of measurement for a unit price measurement.

  - Ml - 1000 milliliters equals 1 liter.
  - Cl - 100 centiliters equals 1 liter.
  - L - Metric system unit of volume.
  - M3 - 1 cubic meter equals 1000 liters.
  - Mg - 1000 milligrams equals 1 gram.
  - G - Metric system unit of weight.
  - Kg - 1 kilogram equals 1000 grams.
  - Mm - 1000 millimeters equals 1 meter.
  - Cm - 100 centimeters equals 1 meter.
  - M - Metric system unit of length.
  - M2 - Metric system unit of area.

-}
type UnitPriceMeasurementMeasuredUnit
    = Ml
    | Cl
    | L
    | M3
    | Mg
    | G
    | Kg
    | Mm
    | Cm
    | M
    | M2


list : List UnitPriceMeasurementMeasuredUnit
list =
    [ Ml, Cl, L, M3, Mg, G, Kg, Mm, Cm, M, M2 ]


decoder : Decoder UnitPriceMeasurementMeasuredUnit
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ML" ->
                        Decode.succeed Ml

                    "CL" ->
                        Decode.succeed Cl

                    "L" ->
                        Decode.succeed L

                    "M3" ->
                        Decode.succeed M3

                    "MG" ->
                        Decode.succeed Mg

                    "G" ->
                        Decode.succeed G

                    "KG" ->
                        Decode.succeed Kg

                    "MM" ->
                        Decode.succeed Mm

                    "CM" ->
                        Decode.succeed Cm

                    "M" ->
                        Decode.succeed M

                    "M2" ->
                        Decode.succeed M2

                    _ ->
                        Decode.fail ("Invalid UnitPriceMeasurementMeasuredUnit type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : UnitPriceMeasurementMeasuredUnit -> String
toString enum____ =
    case enum____ of
        Ml ->
            "ML"

        Cl ->
            "CL"

        L ->
            "L"

        M3 ->
            "M3"

        Mg ->
            "MG"

        G ->
            "G"

        Kg ->
            "KG"

        Mm ->
            "MM"

        Cm ->
            "CM"

        M ->
            "M"

        M2 ->
            "M2"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe UnitPriceMeasurementMeasuredUnit
fromString enumString____ =
    case enumString____ of
        "ML" ->
            Just Ml

        "CL" ->
            Just Cl

        "L" ->
            Just L

        "M3" ->
            Just M3

        "MG" ->
            Just Mg

        "G" ->
            Just G

        "KG" ->
            Just Kg

        "MM" ->
            Just Mm

        "CM" ->
            Just Cm

        "M" ->
            Just M

        "M2" ->
            Just M2

        _ ->
            Nothing
