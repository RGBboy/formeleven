-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Enum.OrderFinancialStatus exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| Represents the order's current financial status.

  - Pending - Displayed as **Pending**.
  - Authorized - Displayed as **Authorized**.
  - PartiallyPaid - Displayed as **Partially paid**.
  - PartiallyRefunded - Displayed as **Partially refunded**.
  - Voided - Displayed as **Voided**.
  - Paid - Displayed as **Paid**.
  - Refunded - Displayed as **Refunded**.

-}
type OrderFinancialStatus
    = Pending
    | Authorized
    | PartiallyPaid
    | PartiallyRefunded
    | Voided
    | Paid
    | Refunded


list : List OrderFinancialStatus
list =
    [ Pending, Authorized, PartiallyPaid, PartiallyRefunded, Voided, Paid, Refunded ]


decoder : Decoder OrderFinancialStatus
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "PENDING" ->
                        Decode.succeed Pending

                    "AUTHORIZED" ->
                        Decode.succeed Authorized

                    "PARTIALLY_PAID" ->
                        Decode.succeed PartiallyPaid

                    "PARTIALLY_REFUNDED" ->
                        Decode.succeed PartiallyRefunded

                    "VOIDED" ->
                        Decode.succeed Voided

                    "PAID" ->
                        Decode.succeed Paid

                    "REFUNDED" ->
                        Decode.succeed Refunded

                    _ ->
                        Decode.fail ("Invalid OrderFinancialStatus type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : OrderFinancialStatus -> String
toString enum____ =
    case enum____ of
        Pending ->
            "PENDING"

        Authorized ->
            "AUTHORIZED"

        PartiallyPaid ->
            "PARTIALLY_PAID"

        PartiallyRefunded ->
            "PARTIALLY_REFUNDED"

        Voided ->
            "VOIDED"

        Paid ->
            "PAID"

        Refunded ->
            "REFUNDED"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe OrderFinancialStatus
fromString enumString____ =
    case enumString____ of
        "PENDING" ->
            Just Pending

        "AUTHORIZED" ->
            Just Authorized

        "PARTIALLY_PAID" ->
            Just PartiallyPaid

        "PARTIALLY_REFUNDED" ->
            Just PartiallyRefunded

        "VOIDED" ->
            Just Voided

        "PAID" ->
            Just Paid

        "REFUNDED" ->
            Just Refunded

        _ ->
            Nothing
