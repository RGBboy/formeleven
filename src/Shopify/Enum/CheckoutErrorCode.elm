-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Enum.CheckoutErrorCode exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| Possible error codes that can be returned by `CheckoutUserError`.

  - Blank - The input value is blank.
  - Invalid - The input value is invalid.
  - TooLong - The input value is too long.
  - Present - The input value needs to be blank.
  - LessThan - The input value should be less than the maximum value allowed.
  - GreaterThanOrEqualTo - The input value should be greater than or equal to the minimum value allowed.
  - LessThanOrEqualTo - The input value should be less than or equal to the maximum value allowed.
  - AlreadyCompleted - Checkout is already completed.
  - Locked - Checkout is locked.
  - NotSupported - Input value is not supported.
  - BadDomain - Input email contains an invalid domain name.
  - InvalidForCountry - Input Zip is invalid for country provided.
  - InvalidForCountryAndProvince - Input Zip is invalid for country and province provided.
  - InvalidStateInCountry - Invalid state in country.
  - InvalidProvinceInCountry - Invalid province in country.
  - InvalidRegionInCountry - Invalid region in country.
  - ShippingRateExpired - Shipping rate expired.
  - GiftCardUnusable - Gift card cannot be applied to a checkout that contains a gift card.
  - GiftCardDisabled - Gift card is disabled.
  - GiftCardCodeInvalid - Gift card code is invalid.
  - GiftCardAlreadyApplied - Gift card has already been applied.
  - GiftCardCurrencyMismatch - Gift card currency does not match checkout currency.
  - GiftCardExpired - Gift card is expired.
  - GiftCardDepleted - Gift card has no funds left.
  - GiftCardNotFound - Gift card was not found.
  - CartDoesNotMeetDiscountRequirementsNotice - Cart does not meet discount requirements notice.
  - DiscountExpired - Discount expired.
  - DiscountDisabled - Discount disabled.
  - DiscountLimitReached - Discount limit reached.
  - DiscountNotFound - Discount not found.
  - CustomerAlreadyUsedOncePerCustomerDiscountNotice - Customer already used once per customer discount notice.
  - Empty - Checkout is already completed.
  - NotEnoughInStock - Not enough in stock.
  - MissingPaymentInput - Missing payment input.
  - TotalPriceMismatch - The amount of the payment does not match the value to be paid.
  - LineItemNotFound - Line item was not found in checkout.
  - UnableToApply - Unable to apply discount.
  - DiscountAlreadyApplied - Discount already applied.
  - ThrottledDuringCheckout - Throttled during checkout.
  - ExpiredQueueToken - Queue token has expired.
  - InvalidQueueToken - Queue token is invalid.
  - InvalidCountryAndCurrency - Cannot specify country and presentment currency code.

-}
type CheckoutErrorCode
    = Blank
    | Invalid
    | TooLong
    | Present
    | LessThan
    | GreaterThanOrEqualTo
    | LessThanOrEqualTo
    | AlreadyCompleted
    | Locked
    | NotSupported
    | BadDomain
    | InvalidForCountry
    | InvalidForCountryAndProvince
    | InvalidStateInCountry
    | InvalidProvinceInCountry
    | InvalidRegionInCountry
    | ShippingRateExpired
    | GiftCardUnusable
    | GiftCardDisabled
    | GiftCardCodeInvalid
    | GiftCardAlreadyApplied
    | GiftCardCurrencyMismatch
    | GiftCardExpired
    | GiftCardDepleted
    | GiftCardNotFound
    | CartDoesNotMeetDiscountRequirementsNotice
    | DiscountExpired
    | DiscountDisabled
    | DiscountLimitReached
    | DiscountNotFound
    | CustomerAlreadyUsedOncePerCustomerDiscountNotice
    | Empty
    | NotEnoughInStock
    | MissingPaymentInput
    | TotalPriceMismatch
    | LineItemNotFound
    | UnableToApply
    | DiscountAlreadyApplied
    | ThrottledDuringCheckout
    | ExpiredQueueToken
    | InvalidQueueToken
    | InvalidCountryAndCurrency


list : List CheckoutErrorCode
list =
    [ Blank, Invalid, TooLong, Present, LessThan, GreaterThanOrEqualTo, LessThanOrEqualTo, AlreadyCompleted, Locked, NotSupported, BadDomain, InvalidForCountry, InvalidForCountryAndProvince, InvalidStateInCountry, InvalidProvinceInCountry, InvalidRegionInCountry, ShippingRateExpired, GiftCardUnusable, GiftCardDisabled, GiftCardCodeInvalid, GiftCardAlreadyApplied, GiftCardCurrencyMismatch, GiftCardExpired, GiftCardDepleted, GiftCardNotFound, CartDoesNotMeetDiscountRequirementsNotice, DiscountExpired, DiscountDisabled, DiscountLimitReached, DiscountNotFound, CustomerAlreadyUsedOncePerCustomerDiscountNotice, Empty, NotEnoughInStock, MissingPaymentInput, TotalPriceMismatch, LineItemNotFound, UnableToApply, DiscountAlreadyApplied, ThrottledDuringCheckout, ExpiredQueueToken, InvalidQueueToken, InvalidCountryAndCurrency ]


decoder : Decoder CheckoutErrorCode
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "BLANK" ->
                        Decode.succeed Blank

                    "INVALID" ->
                        Decode.succeed Invalid

                    "TOO_LONG" ->
                        Decode.succeed TooLong

                    "PRESENT" ->
                        Decode.succeed Present

                    "LESS_THAN" ->
                        Decode.succeed LessThan

                    "GREATER_THAN_OR_EQUAL_TO" ->
                        Decode.succeed GreaterThanOrEqualTo

                    "LESS_THAN_OR_EQUAL_TO" ->
                        Decode.succeed LessThanOrEqualTo

                    "ALREADY_COMPLETED" ->
                        Decode.succeed AlreadyCompleted

                    "LOCKED" ->
                        Decode.succeed Locked

                    "NOT_SUPPORTED" ->
                        Decode.succeed NotSupported

                    "BAD_DOMAIN" ->
                        Decode.succeed BadDomain

                    "INVALID_FOR_COUNTRY" ->
                        Decode.succeed InvalidForCountry

                    "INVALID_FOR_COUNTRY_AND_PROVINCE" ->
                        Decode.succeed InvalidForCountryAndProvince

                    "INVALID_STATE_IN_COUNTRY" ->
                        Decode.succeed InvalidStateInCountry

                    "INVALID_PROVINCE_IN_COUNTRY" ->
                        Decode.succeed InvalidProvinceInCountry

                    "INVALID_REGION_IN_COUNTRY" ->
                        Decode.succeed InvalidRegionInCountry

                    "SHIPPING_RATE_EXPIRED" ->
                        Decode.succeed ShippingRateExpired

                    "GIFT_CARD_UNUSABLE" ->
                        Decode.succeed GiftCardUnusable

                    "GIFT_CARD_DISABLED" ->
                        Decode.succeed GiftCardDisabled

                    "GIFT_CARD_CODE_INVALID" ->
                        Decode.succeed GiftCardCodeInvalid

                    "GIFT_CARD_ALREADY_APPLIED" ->
                        Decode.succeed GiftCardAlreadyApplied

                    "GIFT_CARD_CURRENCY_MISMATCH" ->
                        Decode.succeed GiftCardCurrencyMismatch

                    "GIFT_CARD_EXPIRED" ->
                        Decode.succeed GiftCardExpired

                    "GIFT_CARD_DEPLETED" ->
                        Decode.succeed GiftCardDepleted

                    "GIFT_CARD_NOT_FOUND" ->
                        Decode.succeed GiftCardNotFound

                    "CART_DOES_NOT_MEET_DISCOUNT_REQUIREMENTS_NOTICE" ->
                        Decode.succeed CartDoesNotMeetDiscountRequirementsNotice

                    "DISCOUNT_EXPIRED" ->
                        Decode.succeed DiscountExpired

                    "DISCOUNT_DISABLED" ->
                        Decode.succeed DiscountDisabled

                    "DISCOUNT_LIMIT_REACHED" ->
                        Decode.succeed DiscountLimitReached

                    "DISCOUNT_NOT_FOUND" ->
                        Decode.succeed DiscountNotFound

                    "CUSTOMER_ALREADY_USED_ONCE_PER_CUSTOMER_DISCOUNT_NOTICE" ->
                        Decode.succeed CustomerAlreadyUsedOncePerCustomerDiscountNotice

                    "EMPTY" ->
                        Decode.succeed Empty

                    "NOT_ENOUGH_IN_STOCK" ->
                        Decode.succeed NotEnoughInStock

                    "MISSING_PAYMENT_INPUT" ->
                        Decode.succeed MissingPaymentInput

                    "TOTAL_PRICE_MISMATCH" ->
                        Decode.succeed TotalPriceMismatch

                    "LINE_ITEM_NOT_FOUND" ->
                        Decode.succeed LineItemNotFound

                    "UNABLE_TO_APPLY" ->
                        Decode.succeed UnableToApply

                    "DISCOUNT_ALREADY_APPLIED" ->
                        Decode.succeed DiscountAlreadyApplied

                    "THROTTLED_DURING_CHECKOUT" ->
                        Decode.succeed ThrottledDuringCheckout

                    "EXPIRED_QUEUE_TOKEN" ->
                        Decode.succeed ExpiredQueueToken

                    "INVALID_QUEUE_TOKEN" ->
                        Decode.succeed InvalidQueueToken

                    "INVALID_COUNTRY_AND_CURRENCY" ->
                        Decode.succeed InvalidCountryAndCurrency

                    _ ->
                        Decode.fail ("Invalid CheckoutErrorCode type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : CheckoutErrorCode -> String
toString enum____ =
    case enum____ of
        Blank ->
            "BLANK"

        Invalid ->
            "INVALID"

        TooLong ->
            "TOO_LONG"

        Present ->
            "PRESENT"

        LessThan ->
            "LESS_THAN"

        GreaterThanOrEqualTo ->
            "GREATER_THAN_OR_EQUAL_TO"

        LessThanOrEqualTo ->
            "LESS_THAN_OR_EQUAL_TO"

        AlreadyCompleted ->
            "ALREADY_COMPLETED"

        Locked ->
            "LOCKED"

        NotSupported ->
            "NOT_SUPPORTED"

        BadDomain ->
            "BAD_DOMAIN"

        InvalidForCountry ->
            "INVALID_FOR_COUNTRY"

        InvalidForCountryAndProvince ->
            "INVALID_FOR_COUNTRY_AND_PROVINCE"

        InvalidStateInCountry ->
            "INVALID_STATE_IN_COUNTRY"

        InvalidProvinceInCountry ->
            "INVALID_PROVINCE_IN_COUNTRY"

        InvalidRegionInCountry ->
            "INVALID_REGION_IN_COUNTRY"

        ShippingRateExpired ->
            "SHIPPING_RATE_EXPIRED"

        GiftCardUnusable ->
            "GIFT_CARD_UNUSABLE"

        GiftCardDisabled ->
            "GIFT_CARD_DISABLED"

        GiftCardCodeInvalid ->
            "GIFT_CARD_CODE_INVALID"

        GiftCardAlreadyApplied ->
            "GIFT_CARD_ALREADY_APPLIED"

        GiftCardCurrencyMismatch ->
            "GIFT_CARD_CURRENCY_MISMATCH"

        GiftCardExpired ->
            "GIFT_CARD_EXPIRED"

        GiftCardDepleted ->
            "GIFT_CARD_DEPLETED"

        GiftCardNotFound ->
            "GIFT_CARD_NOT_FOUND"

        CartDoesNotMeetDiscountRequirementsNotice ->
            "CART_DOES_NOT_MEET_DISCOUNT_REQUIREMENTS_NOTICE"

        DiscountExpired ->
            "DISCOUNT_EXPIRED"

        DiscountDisabled ->
            "DISCOUNT_DISABLED"

        DiscountLimitReached ->
            "DISCOUNT_LIMIT_REACHED"

        DiscountNotFound ->
            "DISCOUNT_NOT_FOUND"

        CustomerAlreadyUsedOncePerCustomerDiscountNotice ->
            "CUSTOMER_ALREADY_USED_ONCE_PER_CUSTOMER_DISCOUNT_NOTICE"

        Empty ->
            "EMPTY"

        NotEnoughInStock ->
            "NOT_ENOUGH_IN_STOCK"

        MissingPaymentInput ->
            "MISSING_PAYMENT_INPUT"

        TotalPriceMismatch ->
            "TOTAL_PRICE_MISMATCH"

        LineItemNotFound ->
            "LINE_ITEM_NOT_FOUND"

        UnableToApply ->
            "UNABLE_TO_APPLY"

        DiscountAlreadyApplied ->
            "DISCOUNT_ALREADY_APPLIED"

        ThrottledDuringCheckout ->
            "THROTTLED_DURING_CHECKOUT"

        ExpiredQueueToken ->
            "EXPIRED_QUEUE_TOKEN"

        InvalidQueueToken ->
            "INVALID_QUEUE_TOKEN"

        InvalidCountryAndCurrency ->
            "INVALID_COUNTRY_AND_CURRENCY"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe CheckoutErrorCode
fromString enumString____ =
    case enumString____ of
        "BLANK" ->
            Just Blank

        "INVALID" ->
            Just Invalid

        "TOO_LONG" ->
            Just TooLong

        "PRESENT" ->
            Just Present

        "LESS_THAN" ->
            Just LessThan

        "GREATER_THAN_OR_EQUAL_TO" ->
            Just GreaterThanOrEqualTo

        "LESS_THAN_OR_EQUAL_TO" ->
            Just LessThanOrEqualTo

        "ALREADY_COMPLETED" ->
            Just AlreadyCompleted

        "LOCKED" ->
            Just Locked

        "NOT_SUPPORTED" ->
            Just NotSupported

        "BAD_DOMAIN" ->
            Just BadDomain

        "INVALID_FOR_COUNTRY" ->
            Just InvalidForCountry

        "INVALID_FOR_COUNTRY_AND_PROVINCE" ->
            Just InvalidForCountryAndProvince

        "INVALID_STATE_IN_COUNTRY" ->
            Just InvalidStateInCountry

        "INVALID_PROVINCE_IN_COUNTRY" ->
            Just InvalidProvinceInCountry

        "INVALID_REGION_IN_COUNTRY" ->
            Just InvalidRegionInCountry

        "SHIPPING_RATE_EXPIRED" ->
            Just ShippingRateExpired

        "GIFT_CARD_UNUSABLE" ->
            Just GiftCardUnusable

        "GIFT_CARD_DISABLED" ->
            Just GiftCardDisabled

        "GIFT_CARD_CODE_INVALID" ->
            Just GiftCardCodeInvalid

        "GIFT_CARD_ALREADY_APPLIED" ->
            Just GiftCardAlreadyApplied

        "GIFT_CARD_CURRENCY_MISMATCH" ->
            Just GiftCardCurrencyMismatch

        "GIFT_CARD_EXPIRED" ->
            Just GiftCardExpired

        "GIFT_CARD_DEPLETED" ->
            Just GiftCardDepleted

        "GIFT_CARD_NOT_FOUND" ->
            Just GiftCardNotFound

        "CART_DOES_NOT_MEET_DISCOUNT_REQUIREMENTS_NOTICE" ->
            Just CartDoesNotMeetDiscountRequirementsNotice

        "DISCOUNT_EXPIRED" ->
            Just DiscountExpired

        "DISCOUNT_DISABLED" ->
            Just DiscountDisabled

        "DISCOUNT_LIMIT_REACHED" ->
            Just DiscountLimitReached

        "DISCOUNT_NOT_FOUND" ->
            Just DiscountNotFound

        "CUSTOMER_ALREADY_USED_ONCE_PER_CUSTOMER_DISCOUNT_NOTICE" ->
            Just CustomerAlreadyUsedOncePerCustomerDiscountNotice

        "EMPTY" ->
            Just Empty

        "NOT_ENOUGH_IN_STOCK" ->
            Just NotEnoughInStock

        "MISSING_PAYMENT_INPUT" ->
            Just MissingPaymentInput

        "TOTAL_PRICE_MISMATCH" ->
            Just TotalPriceMismatch

        "LINE_ITEM_NOT_FOUND" ->
            Just LineItemNotFound

        "UNABLE_TO_APPLY" ->
            Just UnableToApply

        "DISCOUNT_ALREADY_APPLIED" ->
            Just DiscountAlreadyApplied

        "THROTTLED_DURING_CHECKOUT" ->
            Just ThrottledDuringCheckout

        "EXPIRED_QUEUE_TOKEN" ->
            Just ExpiredQueueToken

        "INVALID_QUEUE_TOKEN" ->
            Just InvalidQueueToken

        "INVALID_COUNTRY_AND_CURRENCY" ->
            Just InvalidCountryAndCurrency

        _ ->
            Nothing
