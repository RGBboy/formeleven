module Money exposing (..)

import Decimal exposing (Decimal)
import Round

-- Eventually change this to preserve the currency type from the API
type alias Money =
  { amount: Decimal
  , currencyCode : String
  }

-- TODO: cater for currency codes
add : Money -> Money -> Money
add moneyA moneyB =
  { moneyA | amount = Decimal.add moneyA.amount moneyB.amount }

-- TODO: cater for currency codes
sub : Money -> Money -> Money
sub moneyA moneyB =
  { moneyA | amount = Decimal.sub moneyA.amount moneyB.amount }

mul : Money -> Int -> Money
mul money factor =
  { money | amount = Decimal.mul money.amount (Decimal.fromInt factor) }

-- TODO: cater for currency codes
toString : Money -> String
toString money =
  let
    stringAmount = money.amount
      |> Decimal.toFloat
      |> Round.round 2
  in
    "£" ++ stringAmount
