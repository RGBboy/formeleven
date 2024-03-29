-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Enum.CartErrorCode exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| Possible error codes that can be returned by `CartUserError`.

  - Invalid - The input value is invalid.
  - LessThan - The input value should be less than the maximum value allowed.
  - InvalidMerchandiseLine - Merchandise line was not found in cart.
  - MissingDiscountCode - Missing discount code.
  - MissingNote - Missing note.

-}
type CartErrorCode
    = Invalid
    | LessThan
    | InvalidMerchandiseLine
    | MissingDiscountCode
    | MissingNote


list : List CartErrorCode
list =
    [ Invalid, LessThan, InvalidMerchandiseLine, MissingDiscountCode, MissingNote ]


decoder : Decoder CartErrorCode
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "INVALID" ->
                        Decode.succeed Invalid

                    "LESS_THAN" ->
                        Decode.succeed LessThan

                    "INVALID_MERCHANDISE_LINE" ->
                        Decode.succeed InvalidMerchandiseLine

                    "MISSING_DISCOUNT_CODE" ->
                        Decode.succeed MissingDiscountCode

                    "MISSING_NOTE" ->
                        Decode.succeed MissingNote

                    _ ->
                        Decode.fail ("Invalid CartErrorCode type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : CartErrorCode -> String
toString enum____ =
    case enum____ of
        Invalid ->
            "INVALID"

        LessThan ->
            "LESS_THAN"

        InvalidMerchandiseLine ->
            "INVALID_MERCHANDISE_LINE"

        MissingDiscountCode ->
            "MISSING_DISCOUNT_CODE"

        MissingNote ->
            "MISSING_NOTE"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe CartErrorCode
fromString enumString____ =
    case enumString____ of
        "INVALID" ->
            Just Invalid

        "LESS_THAN" ->
            Just LessThan

        "INVALID_MERCHANDISE_LINE" ->
            Just InvalidMerchandiseLine

        "MISSING_DISCOUNT_CODE" ->
            Just MissingDiscountCode

        "MISSING_NOTE" ->
            Just MissingNote

        _ ->
            Nothing
