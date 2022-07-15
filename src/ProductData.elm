module ProductData exposing (..)

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

decodeKeyValue : String -> Decoder (String, String)
decodeKeyValue value =
  case String.split ": " value of
    [k, v] -> Decode.succeed (k, v)
    _ -> Decode.fail "String does not contain a single \":\" delimeter"

productDetailsDecoder : Decoder (List (String, String))
productDetailsDecoder =
  Decode.string
    |> Decode.andThen decodeKeyValue
    |> Decode.list
