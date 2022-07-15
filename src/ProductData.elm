module ProductData exposing (..)

import Length exposing (Length)
import Json.Decode as Decode exposing (Decoder)



-- Todo: Change this type to an actual monetary type.
-- Perhaps move to use a JSON.Value for flags then decode to
-- correctly handle input data and errors
type alias Money =
  { amount: String
  , currencyCode : String
  }

type alias Image =
  { url : String -- Ideally this should be type URL
  }

type alias NodeList a =
  { nodes: List a
  }

nodeListList : NodeList a -> List a
nodeListList { nodes } = nodes

type alias Metafield =
  { value : String
  }

decodeMetafield : Decoder a -> Metafield -> Maybe a
decodeMetafield decoder field =
  Decode.decodeString decoder field.value
    |> Result.toMaybe

productHighlightsDecoder : Decoder (List String)
productHighlightsDecoder =
  Decode.list Decode.string

keyValueDecoder : String -> Decoder (String, String)
keyValueDecoder value =
  case String.split ": " value of
    [k, v] -> Decode.succeed (k, v)
    _ -> Decode.fail "String does not contain a single \":\" delimeter"

productDetailsDecoder : Decoder (List (String, String))
productDetailsDecoder =
  Decode.string
    |> Decode.andThen keyValueDecoder
    |> Decode.list

type alias Dimensions =
  { width : Length
  , depth : Length
  , height : Length
  }

type alias LightData =
  { colour : String
  , cordLength : Length
  , source : String
  }

-- productMetadata
type alias Metadata =
  { colour : Maybe String
  , dimensions : Maybe Dimensions
  , light : Maybe LightData
  , materials : Maybe (List String)
  }

decodeLength : (String, Int) -> Decoder Length
decodeLength (unit, value) =
  case unit of
    "mm" ->
      value
       |> toFloat
       |> Length.millimeters
       |> Decode.succeed
    "m" ->
      value
       |> toFloat
       |> Length.meters
       |> Decode.succeed
    _ -> Decode.fail ("Unsupported unit " ++ unit)

lengthDecoder : Decoder Length
lengthDecoder =
  Decode.map2 Tuple.pair
    (Decode.field "unit" Decode.string)
    (Decode.field "value" Decode.int)
  |> Decode.andThen decodeLength

dimensionsDecoder : Decoder Dimensions
dimensionsDecoder =
  Decode.map3 Dimensions
    (Decode.field "width" lengthDecoder)
    (Decode.field "depth" lengthDecoder)
    (Decode.field "height" lengthDecoder)

lightDataDecoder : Decoder LightData
lightDataDecoder =
  Decode.map3 LightData
    (Decode.field "colour" Decode.string)
    (Decode.field "cordLength" lengthDecoder)
    (Decode.field "source" Decode.string)

productMetadataDecoder : Decoder Metadata
productMetadataDecoder =
  Decode.map4 Metadata
    (Decode.maybe (Decode.field "colour" Decode.string))
    (Decode.maybe (Decode.field "dimensions" dimensionsDecoder))
    (Decode.maybe (Decode.field "light" lightDataDecoder))
    (Decode.maybe (Decode.field "materials" (Decode.list Decode.string)))
