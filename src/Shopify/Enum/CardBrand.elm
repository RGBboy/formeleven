-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Enum.CardBrand exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| Card brand, such as Visa or Mastercard, which can be used for payments.

  - Visa - Visa.
  - Mastercard - Mastercard.
  - Discover - Discover.
  - AmericanExpress - American Express.
  - DinersClub - Diners Club.
  - Jcb - JCB.

-}
type CardBrand
    = Visa
    | Mastercard
    | Discover
    | AmericanExpress
    | DinersClub
    | Jcb


list : List CardBrand
list =
    [ Visa, Mastercard, Discover, AmericanExpress, DinersClub, Jcb ]


decoder : Decoder CardBrand
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "VISA" ->
                        Decode.succeed Visa

                    "MASTERCARD" ->
                        Decode.succeed Mastercard

                    "DISCOVER" ->
                        Decode.succeed Discover

                    "AMERICAN_EXPRESS" ->
                        Decode.succeed AmericanExpress

                    "DINERS_CLUB" ->
                        Decode.succeed DinersClub

                    "JCB" ->
                        Decode.succeed Jcb

                    _ ->
                        Decode.fail ("Invalid CardBrand type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : CardBrand -> String
toString enum____ =
    case enum____ of
        Visa ->
            "VISA"

        Mastercard ->
            "MASTERCARD"

        Discover ->
            "DISCOVER"

        AmericanExpress ->
            "AMERICAN_EXPRESS"

        DinersClub ->
            "DINERS_CLUB"

        Jcb ->
            "JCB"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe CardBrand
fromString enumString____ =
    case enumString____ of
        "VISA" ->
            Just Visa

        "MASTERCARD" ->
            Just Mastercard

        "DISCOVER" ->
            Just Discover

        "AMERICAN_EXPRESS" ->
            Just AmericanExpress

        "DINERS_CLUB" ->
            Just DinersClub

        "JCB" ->
            Just Jcb

        _ ->
            Nothing
