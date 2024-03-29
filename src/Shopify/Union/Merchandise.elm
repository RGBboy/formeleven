-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Union.Merchandise exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (FragmentSelectionSet(..), SelectionSet(..))
import Json.Decode as Decode
import Shopify.InputObject
import Shopify.Interface
import Shopify.Object
import Shopify.Scalar
import Shopify.ScalarCodecs
import Shopify.Union


type alias Fragments decodesTo =
    { onProductVariant : SelectionSet decodesTo Shopify.Object.ProductVariant
    }


{-| Build up a selection for this Union by passing in a Fragments record.
-}
fragments :
    Fragments decodesTo
    -> SelectionSet decodesTo Shopify.Union.Merchandise
fragments selections____ =
    Object.exhaustiveFragmentSelection
        [ Object.buildFragment "ProductVariant" selections____.onProductVariant
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    { onProductVariant = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }
