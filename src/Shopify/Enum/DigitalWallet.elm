-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Enum.DigitalWallet exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| Digital wallet, such as Apple Pay, which can be used for accelerated checkouts.

  - ApplePay - Apple Pay.
  - AndroidPay - Android Pay.
  - GooglePay - Google Pay.
  - ShopifyPay - Shopify Pay.

-}
type DigitalWallet
    = ApplePay
    | AndroidPay
    | GooglePay
    | ShopifyPay


list : List DigitalWallet
list =
    [ ApplePay, AndroidPay, GooglePay, ShopifyPay ]


decoder : Decoder DigitalWallet
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "APPLE_PAY" ->
                        Decode.succeed ApplePay

                    "ANDROID_PAY" ->
                        Decode.succeed AndroidPay

                    "GOOGLE_PAY" ->
                        Decode.succeed GooglePay

                    "SHOPIFY_PAY" ->
                        Decode.succeed ShopifyPay

                    _ ->
                        Decode.fail ("Invalid DigitalWallet type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : DigitalWallet -> String
toString enum____ =
    case enum____ of
        ApplePay ->
            "APPLE_PAY"

        AndroidPay ->
            "ANDROID_PAY"

        GooglePay ->
            "GOOGLE_PAY"

        ShopifyPay ->
            "SHOPIFY_PAY"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe DigitalWallet
fromString enumString____ =
    case enumString____ of
        "APPLE_PAY" ->
            Just ApplePay

        "ANDROID_PAY" ->
            Just AndroidPay

        "GOOGLE_PAY" ->
            Just GooglePay

        "SHOPIFY_PAY" ->
            Just ShopifyPay

        _ ->
            Nothing
