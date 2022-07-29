-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.Order exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Shopify.Enum.CurrencyCode
import Shopify.Enum.OrderCancelReason
import Shopify.Enum.OrderFinancialStatus
import Shopify.Enum.OrderFulfillmentStatus
import Shopify.InputObject
import Shopify.Interface
import Shopify.Object
import Shopify.Scalar
import Shopify.ScalarCodecs
import Shopify.Union


{-| The reason for the order's cancellation. Returns `null` if the order wasn't canceled.
-}
cancelReason : SelectionSet (Maybe Shopify.Enum.OrderCancelReason.OrderCancelReason) Shopify.Object.Order
cancelReason =
    Object.selectionForField "(Maybe Enum.OrderCancelReason.OrderCancelReason)" "cancelReason" [] (Shopify.Enum.OrderCancelReason.decoder |> Decode.nullable)


{-| The date and time when the order was canceled. Returns null if the order wasn't canceled.
-}
canceledAt : SelectionSet (Maybe Shopify.ScalarCodecs.DateTime) Shopify.Object.Order
canceledAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "canceledAt" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| The code of the currency used for the payment.
-}
currencyCode : SelectionSet Shopify.Enum.CurrencyCode.CurrencyCode Shopify.Object.Order
currencyCode =
    Object.selectionForField "Enum.CurrencyCode.CurrencyCode" "currencyCode" [] Shopify.Enum.CurrencyCode.decoder


{-| The subtotal of line items and their discounts, excluding line items that have been removed. Does not contain order-level discounts, duties, shipping costs, or shipping discounts. Taxes are not included unless the order is a taxes-included order.
-}
currentSubtotalPrice :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet decodesTo Shopify.Object.Order
currentSubtotalPrice object____ =
    Object.selectionForCompositeField "currentSubtotalPrice" [] object____ Basics.identity


{-| The total cost of duties for the order, including refunds.
-}
currentTotalDuties :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet (Maybe decodesTo) Shopify.Object.Order
currentTotalDuties object____ =
    Object.selectionForCompositeField "currentTotalDuties" [] object____ (Basics.identity >> Decode.nullable)


{-| The total amount of the order, including duties, taxes and discounts, minus amounts for line items that have been removed.
-}
currentTotalPrice :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet decodesTo Shopify.Object.Order
currentTotalPrice object____ =
    Object.selectionForCompositeField "currentTotalPrice" [] object____ Basics.identity


{-| The total of all taxes applied to the order, excluding taxes for returned line items.
-}
currentTotalTax :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet decodesTo Shopify.Object.Order
currentTotalTax object____ =
    Object.selectionForCompositeField "currentTotalTax" [] object____ Basics.identity


{-| The locale code in which this specific order happened.
-}
customerLocale : SelectionSet (Maybe String) Shopify.Object.Order
customerLocale =
    Object.selectionForField "(Maybe String)" "customerLocale" [] (Decode.string |> Decode.nullable)


{-| The unique URL that the customer can use to access the order.
-}
customerUrl : SelectionSet (Maybe Shopify.ScalarCodecs.Url) Shopify.Object.Order
customerUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Url)" "customerUrl" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecUrl |> .decoder |> Decode.nullable)


type alias DiscountApplicationsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , reverse : OptionalArgument Bool
    }


{-| Discounts that have been applied on the order.

  - first - Returns up to the first `n` elements from the list.
  - after - Returns the elements that come after the specified cursor.
  - last - Returns up to the last `n` elements from the list.
  - before - Returns the elements that come before the specified cursor.
  - reverse - Reverse the order of the underlying list.

-}
discountApplications :
    (DiscountApplicationsOptionalArguments -> DiscountApplicationsOptionalArguments)
    -> SelectionSet decodesTo Shopify.Object.DiscountApplicationConnection
    -> SelectionSet decodesTo Shopify.Object.Order
discountApplications fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, reverse = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "reverse" filledInOptionals____.reverse Encode.bool ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "discountApplications" optionalArgs____ object____ Basics.identity


{-| Whether the order has had any edits applied or not.
-}
edited : SelectionSet Bool Shopify.Object.Order
edited =
    Object.selectionForField "Bool" "edited" [] Decode.bool


{-| The customer's email address.
-}
email : SelectionSet (Maybe String) Shopify.Object.Order
email =
    Object.selectionForField "(Maybe String)" "email" [] (Decode.string |> Decode.nullable)


{-| The financial status of the order.
-}
financialStatus : SelectionSet (Maybe Shopify.Enum.OrderFinancialStatus.OrderFinancialStatus) Shopify.Object.Order
financialStatus =
    Object.selectionForField "(Maybe Enum.OrderFinancialStatus.OrderFinancialStatus)" "financialStatus" [] (Shopify.Enum.OrderFinancialStatus.decoder |> Decode.nullable)


{-| The fulfillment status for the order.
-}
fulfillmentStatus : SelectionSet Shopify.Enum.OrderFulfillmentStatus.OrderFulfillmentStatus Shopify.Object.Order
fulfillmentStatus =
    Object.selectionForField "Enum.OrderFulfillmentStatus.OrderFulfillmentStatus" "fulfillmentStatus" [] Shopify.Enum.OrderFulfillmentStatus.decoder


{-| A globally-unique identifier.
-}
id : SelectionSet Shopify.ScalarCodecs.Id Shopify.Object.Order
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecId |> .decoder)


type alias LineItemsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , reverse : OptionalArgument Bool
    }


{-| List of the order’s line items.

  - first - Returns up to the first `n` elements from the list.
  - after - Returns the elements that come after the specified cursor.
  - last - Returns up to the last `n` elements from the list.
  - before - Returns the elements that come before the specified cursor.
  - reverse - Reverse the order of the underlying list.

-}
lineItems :
    (LineItemsOptionalArguments -> LineItemsOptionalArguments)
    -> SelectionSet decodesTo Shopify.Object.OrderLineItemConnection
    -> SelectionSet decodesTo Shopify.Object.Order
lineItems fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, reverse = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "reverse" filledInOptionals____.reverse Encode.bool ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "lineItems" optionalArgs____ object____ Basics.identity


type alias MetafieldRequiredArguments =
    { namespace : String
    , key : String
    }


{-| Returns a metafield found by namespace and key.

  - namespace - A container for a set of metafields.
  - key - The identifier for the metafield.

-}
metafield :
    MetafieldRequiredArguments
    -> SelectionSet decodesTo Shopify.Object.Metafield
    -> SelectionSet (Maybe decodesTo) Shopify.Object.Order
metafield requiredArgs____ object____ =
    Object.selectionForCompositeField "metafield" [ Argument.required "namespace" requiredArgs____.namespace Encode.string, Argument.required "key" requiredArgs____.key Encode.string ] object____ (Basics.identity >> Decode.nullable)


type alias MetafieldsRequiredArguments =
    { identifiers : List Shopify.InputObject.HasMetafieldsIdentifier }


{-| The metafields associated with the resource matching the supplied list of namespaces and keys.

  - identifiers - The list of metafields to retrieve by namespace and key.

-}
metafields :
    MetafieldsRequiredArguments
    -> SelectionSet decodesTo Shopify.Object.Metafield
    -> SelectionSet (List (Maybe decodesTo)) Shopify.Object.Order
metafields requiredArgs____ object____ =
    Object.selectionForCompositeField "metafields" [ Argument.required "identifiers" requiredArgs____.identifiers (Shopify.InputObject.encodeHasMetafieldsIdentifier |> Encode.list) ] object____ (Basics.identity >> Decode.nullable >> Decode.list)


{-| Unique identifier for the order that appears on the order.
For example, _#1000_ or \_Store1001.
-}
name : SelectionSet String Shopify.Object.Order
name =
    Object.selectionForField "String" "name" [] Decode.string


{-| A unique numeric identifier for the order for use by shop owner and customer.
-}
orderNumber : SelectionSet Int Shopify.Object.Order
orderNumber =
    Object.selectionForField "Int" "orderNumber" [] Decode.int


{-| The total cost of duties charged at checkout.
-}
originalTotalDuties :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet (Maybe decodesTo) Shopify.Object.Order
originalTotalDuties object____ =
    Object.selectionForCompositeField "originalTotalDuties" [] object____ (Basics.identity >> Decode.nullable)


{-| The total price of the order before any applied edits.
-}
originalTotalPrice :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet decodesTo Shopify.Object.Order
originalTotalPrice object____ =
    Object.selectionForCompositeField "originalTotalPrice" [] object____ Basics.identity


{-| The customer's phone number for receiving SMS notifications.
-}
phone : SelectionSet (Maybe String) Shopify.Object.Order
phone =
    Object.selectionForField "(Maybe String)" "phone" [] (Decode.string |> Decode.nullable)


{-| The date and time when the order was imported.
This value can be set to dates in the past when importing from other systems.
If no value is provided, it will be auto-generated based on current date and time.
-}
processedAt : SelectionSet Shopify.ScalarCodecs.DateTime Shopify.Object.Order
processedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "processedAt" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The address to where the order will be shipped.
-}
shippingAddress :
    SelectionSet decodesTo Shopify.Object.MailingAddress
    -> SelectionSet (Maybe decodesTo) Shopify.Object.Order
shippingAddress object____ =
    Object.selectionForCompositeField "shippingAddress" [] object____ (Basics.identity >> Decode.nullable)


{-| The discounts that have been allocated onto the shipping line by discount applications.
-}
shippingDiscountAllocations :
    SelectionSet decodesTo Shopify.Object.DiscountAllocation
    -> SelectionSet (List decodesTo) Shopify.Object.Order
shippingDiscountAllocations object____ =
    Object.selectionForCompositeField "shippingDiscountAllocations" [] object____ (Basics.identity >> Decode.list)


{-| The unique URL for the order's status page.
-}
statusUrl : SelectionSet Shopify.ScalarCodecs.Url Shopify.Object.Order
statusUrl =
    Object.selectionForField "ScalarCodecs.Url" "statusUrl" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecUrl |> .decoder)


{-| Price of the order before shipping and taxes.
-}
subtotalPrice : SelectionSet (Maybe Shopify.ScalarCodecs.Money) Shopify.Object.Order
subtotalPrice =
    Object.selectionForField "(Maybe ScalarCodecs.Money)" "subtotalPrice" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecMoney |> .decoder |> Decode.nullable)


{-| Price of the order before duties, shipping and taxes.
-}
subtotalPriceV2 :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet (Maybe decodesTo) Shopify.Object.Order
subtotalPriceV2 object____ =
    Object.selectionForCompositeField "subtotalPriceV2" [] object____ (Basics.identity >> Decode.nullable)


type alias SuccessfulFulfillmentsOptionalArguments =
    { first : OptionalArgument Int }


{-| List of the order’s successful fulfillments.

  - first - Truncate the array result to this size.

-}
successfulFulfillments :
    (SuccessfulFulfillmentsOptionalArguments -> SuccessfulFulfillmentsOptionalArguments)
    -> SelectionSet decodesTo Shopify.Object.Fulfillment
    -> SelectionSet (Maybe (List decodesTo)) Shopify.Object.Order
successfulFulfillments fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "successfulFulfillments" optionalArgs____ object____ (Basics.identity >> Decode.list >> Decode.nullable)


{-| The sum of all the prices of all the items in the order, taxes and discounts included (must be positive).
-}
totalPrice : SelectionSet Shopify.ScalarCodecs.Money Shopify.Object.Order
totalPrice =
    Object.selectionForField "ScalarCodecs.Money" "totalPrice" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecMoney |> .decoder)


{-| The sum of all the prices of all the items in the order, duties, taxes and discounts included (must be positive).
-}
totalPriceV2 :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet decodesTo Shopify.Object.Order
totalPriceV2 object____ =
    Object.selectionForCompositeField "totalPriceV2" [] object____ Basics.identity


{-| The total amount that has been refunded.
-}
totalRefunded : SelectionSet Shopify.ScalarCodecs.Money Shopify.Object.Order
totalRefunded =
    Object.selectionForField "ScalarCodecs.Money" "totalRefunded" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecMoney |> .decoder)


{-| The total amount that has been refunded.
-}
totalRefundedV2 :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet decodesTo Shopify.Object.Order
totalRefundedV2 object____ =
    Object.selectionForCompositeField "totalRefundedV2" [] object____ Basics.identity


{-| The total cost of shipping.
-}
totalShippingPrice : SelectionSet Shopify.ScalarCodecs.Money Shopify.Object.Order
totalShippingPrice =
    Object.selectionForField "ScalarCodecs.Money" "totalShippingPrice" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecMoney |> .decoder)


{-| The total cost of shipping.
-}
totalShippingPriceV2 :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet decodesTo Shopify.Object.Order
totalShippingPriceV2 object____ =
    Object.selectionForCompositeField "totalShippingPriceV2" [] object____ Basics.identity


{-| The total cost of taxes.
-}
totalTax : SelectionSet (Maybe Shopify.ScalarCodecs.Money) Shopify.Object.Order
totalTax =
    Object.selectionForField "(Maybe ScalarCodecs.Money)" "totalTax" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecMoney |> .decoder |> Decode.nullable)


{-| The total cost of taxes.
-}
totalTaxV2 :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet (Maybe decodesTo) Shopify.Object.Order
totalTaxV2 object____ =
    Object.selectionForCompositeField "totalTaxV2" [] object____ (Basics.identity >> Decode.nullable)
