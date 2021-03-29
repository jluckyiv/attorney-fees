module Rate exposing
    ( Rate
    , fromDecimal
    , fromFloat
    , fromString
    , toDecimal
    , toString
    , zero
    )

import Decimal exposing (Decimal)
import Helpers


type Rate
    = Rate Decimal


zero : Rate
zero =
    Rate Decimal.zero


fromString : String -> Maybe Rate
fromString string =
    string
        |> Helpers.stringToDecimal
        |> Maybe.map fromDecimal


fromDecimal : Decimal -> Rate
fromDecimal decimal =
    decimal
        |> Decimal.mul onePercent
        |> Rate


fromFloat : Float -> Maybe Rate
fromFloat float =
    float
        |> Decimal.fromFloat
        |> Maybe.map fromDecimal


toString : Rate -> String
toString rate_ =
    rate_
        |> toDecimal
        |> Decimal.mul (Decimal.fromInt 100)
        |> Decimal.toFloat
        |> String.fromFloat


toDecimal : Rate -> Decimal
toDecimal (Rate decimal) =
    decimal


onePercent : Decimal
onePercent =
    toPercent 1


toPercent : Int -> Decimal
toPercent int =
    Decimal.fromIntWithExponent int -2
