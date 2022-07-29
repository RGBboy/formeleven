-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Object.ProductVariant exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Shopify.Enum.WeightUnit
import Shopify.InputObject
import Shopify.Interface
import Shopify.Object
import Shopify.Scalar
import Shopify.ScalarCodecs
import Shopify.Union


{-| Indicates if the product variant is available for sale.
-}
availableForSale : SelectionSet Bool Shopify.Object.ProductVariant
availableForSale =
    Object.selectionForField "Bool" "availableForSale" [] Decode.bool


{-| The barcode (for example, ISBN, UPC, or GTIN) associated with the variant.
-}
barcode : SelectionSet (Maybe String) Shopify.Object.ProductVariant
barcode =
    Object.selectionForField "(Maybe String)" "barcode" [] (Decode.string |> Decode.nullable)


{-| The compare at price of the variant. This can be used to mark a variant as on sale, when `compareAtPrice` is higher than `price`.
-}
compareAtPrice : SelectionSet (Maybe Shopify.ScalarCodecs.Money) Shopify.Object.ProductVariant
compareAtPrice =
    Object.selectionForField "(Maybe ScalarCodecs.Money)" "compareAtPrice" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecMoney |> .decoder |> Decode.nullable)


{-| The compare at price of the variant. This can be used to mark a variant as on sale, when `compareAtPriceV2` is higher than `priceV2`.
-}
compareAtPriceV2 :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet (Maybe decodesTo) Shopify.Object.ProductVariant
compareAtPriceV2 object____ =
    Object.selectionForCompositeField "compareAtPriceV2" [] object____ (Basics.identity >> Decode.nullable)


{-| Whether a product is out of stock but still available for purchase (used for backorders).
-}
currentlyNotInStock : SelectionSet Bool Shopify.Object.ProductVariant
currentlyNotInStock =
    Object.selectionForField "Bool" "currentlyNotInStock" [] Decode.bool


{-| A globally-unique identifier.
-}
id : SelectionSet Shopify.ScalarCodecs.Id Shopify.Object.ProductVariant
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Image associated with the product variant. This field falls back to the product image if no image is available.
-}
image :
    SelectionSet decodesTo Shopify.Object.Image
    -> SelectionSet (Maybe decodesTo) Shopify.Object.ProductVariant
image object____ =
    Object.selectionForCompositeField "image" [] object____ (Basics.identity >> Decode.nullable)


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
    -> SelectionSet (Maybe decodesTo) Shopify.Object.ProductVariant
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
    -> SelectionSet (List (Maybe decodesTo)) Shopify.Object.ProductVariant
metafields requiredArgs____ object____ =
    Object.selectionForCompositeField "metafields" [ Argument.required "identifiers" requiredArgs____.identifiers (Shopify.InputObject.encodeHasMetafieldsIdentifier |> Encode.list) ] object____ (Basics.identity >> Decode.nullable >> Decode.list)


{-| The product variant’s price.
-}
price : SelectionSet Shopify.ScalarCodecs.Money Shopify.Object.ProductVariant
price =
    Object.selectionForField "ScalarCodecs.Money" "price" [] (Shopify.ScalarCodecs.codecs |> Shopify.Scalar.unwrapCodecs |> .codecMoney |> .decoder)


{-| The product variant’s price.
-}
priceV2 :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet decodesTo Shopify.Object.ProductVariant
priceV2 object____ =
    Object.selectionForCompositeField "priceV2" [] object____ Basics.identity


{-| The product object that the product variant belongs to.
-}
product :
    SelectionSet decodesTo Shopify.Object.Product
    -> SelectionSet decodesTo Shopify.Object.ProductVariant
product object____ =
    Object.selectionForCompositeField "product" [] object____ Basics.identity


{-| The total sellable quantity of the variant for online sales channels.
-}
quantityAvailable : SelectionSet (Maybe Int) Shopify.Object.ProductVariant
quantityAvailable =
    Object.selectionForField "(Maybe Int)" "quantityAvailable" [] (Decode.int |> Decode.nullable)


{-| Whether a customer needs to provide a shipping address when placing an order for the product variant.
-}
requiresShipping : SelectionSet Bool Shopify.Object.ProductVariant
requiresShipping =
    Object.selectionForField "Bool" "requiresShipping" [] Decode.bool


{-| List of product options applied to the variant.
-}
selectedOptions :
    SelectionSet decodesTo Shopify.Object.SelectedOption
    -> SelectionSet (List decodesTo) Shopify.Object.ProductVariant
selectedOptions object____ =
    Object.selectionForCompositeField "selectedOptions" [] object____ (Basics.identity >> Decode.list)


type alias SellingPlanAllocationsOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , reverse : OptionalArgument Bool
    }


{-| Represents an association between a variant and a selling plan. Selling plan allocations describe which selling plans are available for each variant, and what their impact is on pricing.

  - first - Returns up to the first `n` elements from the list.
  - after - Returns the elements that come after the specified cursor.
  - last - Returns up to the last `n` elements from the list.
  - before - Returns the elements that come before the specified cursor.
  - reverse - Reverse the order of the underlying list.

-}
sellingPlanAllocations :
    (SellingPlanAllocationsOptionalArguments -> SellingPlanAllocationsOptionalArguments)
    -> SelectionSet decodesTo Shopify.Object.SellingPlanAllocationConnection
    -> SelectionSet decodesTo Shopify.Object.ProductVariant
sellingPlanAllocations fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, reverse = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "reverse" filledInOptionals____.reverse Encode.bool ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "sellingPlanAllocations" optionalArgs____ object____ Basics.identity


{-| The SKU (stock keeping unit) associated with the variant.
-}
sku : SelectionSet (Maybe String) Shopify.Object.ProductVariant
sku =
    Object.selectionForField "(Maybe String)" "sku" [] (Decode.string |> Decode.nullable)


type alias StoreAvailabilityOptionalArguments =
    { first : OptionalArgument Int
    , after : OptionalArgument String
    , last : OptionalArgument Int
    , before : OptionalArgument String
    , reverse : OptionalArgument Bool
    }


{-| The in-store pickup availability of this variant by location.

  - first - Returns up to the first `n` elements from the list.
  - after - Returns the elements that come after the specified cursor.
  - last - Returns up to the last `n` elements from the list.
  - before - Returns the elements that come before the specified cursor.
  - reverse - Reverse the order of the underlying list.

-}
storeAvailability :
    (StoreAvailabilityOptionalArguments -> StoreAvailabilityOptionalArguments)
    -> SelectionSet decodesTo Shopify.Object.StoreAvailabilityConnection
    -> SelectionSet decodesTo Shopify.Object.ProductVariant
storeAvailability fillInOptionals____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { first = Absent, after = Absent, last = Absent, before = Absent, reverse = Absent }

        optionalArgs____ =
            [ Argument.optional "first" filledInOptionals____.first Encode.int, Argument.optional "after" filledInOptionals____.after Encode.string, Argument.optional "last" filledInOptionals____.last Encode.int, Argument.optional "before" filledInOptionals____.before Encode.string, Argument.optional "reverse" filledInOptionals____.reverse Encode.bool ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "storeAvailability" optionalArgs____ object____ Basics.identity


{-| The product variant’s title.
-}
title : SelectionSet String Shopify.Object.ProductVariant
title =
    Object.selectionForField "String" "title" [] Decode.string


{-| The unit price value for the variant based on the variant's measurement.
-}
unitPrice :
    SelectionSet decodesTo Shopify.Object.MoneyV2
    -> SelectionSet (Maybe decodesTo) Shopify.Object.ProductVariant
unitPrice object____ =
    Object.selectionForCompositeField "unitPrice" [] object____ (Basics.identity >> Decode.nullable)


{-| The unit price measurement for the variant.
-}
unitPriceMeasurement :
    SelectionSet decodesTo Shopify.Object.UnitPriceMeasurement
    -> SelectionSet (Maybe decodesTo) Shopify.Object.ProductVariant
unitPriceMeasurement object____ =
    Object.selectionForCompositeField "unitPriceMeasurement" [] object____ (Basics.identity >> Decode.nullable)


{-| The weight of the product variant in the unit system specified with `weight_unit`.
-}
weight : SelectionSet (Maybe Float) Shopify.Object.ProductVariant
weight =
    Object.selectionForField "(Maybe Float)" "weight" [] (Decode.float |> Decode.nullable)


{-| Unit of measurement for weight.
-}
weightUnit : SelectionSet Shopify.Enum.WeightUnit.WeightUnit Shopify.Object.ProductVariant
weightUnit =
    Object.selectionForField "Enum.WeightUnit.WeightUnit" "weightUnit" [] Shopify.Enum.WeightUnit.decoder
