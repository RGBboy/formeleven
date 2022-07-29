-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Enum.PaymentTokenType exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| The valid values for the types of payment token.

  - ApplePay - Apple Pay token type.
  - Vault - Vault payment token type.
  - ShopifyPay - Shopify Pay token type.
  - GooglePay - Google Pay token type.
  - StripeVaultToken - Stripe token type.

-}
type PaymentTokenType
    = ApplePay
    | Vault
    | ShopifyPay
    | GooglePay
    | StripeVaultToken


list : List PaymentTokenType
list =
    [ ApplePay, Vault, ShopifyPay, GooglePay, StripeVaultToken ]


decoder : Decoder PaymentTokenType
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "APPLE_PAY" ->
                        Decode.succeed ApplePay

                    "VAULT" ->
                        Decode.succeed Vault

                    "SHOPIFY_PAY" ->
                        Decode.succeed ShopifyPay

                    "GOOGLE_PAY" ->
                        Decode.succeed GooglePay

                    "STRIPE_VAULT_TOKEN" ->
                        Decode.succeed StripeVaultToken

                    _ ->
                        Decode.fail ("Invalid PaymentTokenType type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : PaymentTokenType -> String
toString enum____ =
    case enum____ of
        ApplePay ->
            "APPLE_PAY"

        Vault ->
            "VAULT"

        ShopifyPay ->
            "SHOPIFY_PAY"

        GooglePay ->
            "GOOGLE_PAY"

        StripeVaultToken ->
            "STRIPE_VAULT_TOKEN"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe PaymentTokenType
fromString enumString____ =
    case enumString____ of
        "APPLE_PAY" ->
            Just ApplePay

        "VAULT" ->
            Just Vault

        "SHOPIFY_PAY" ->
            Just ShopifyPay

        "GOOGLE_PAY" ->
            Just GooglePay

        "STRIPE_VAULT_TOKEN" ->
            Just StripeVaultToken

        _ ->
            Nothing
