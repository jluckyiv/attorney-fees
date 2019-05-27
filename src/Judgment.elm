module Judgment exposing
    ( Judgment
    , fromDecimal
    , fromFloat
    , fromString
    , toDecimal
    , toString
    , zero
    )

import Decimal exposing (Decimal)
import Helpers


type Judgment
    = Judgment Decimal


zero : Judgment
zero =
    Judgment Decimal.zero


fromString : String -> Maybe Judgment
fromString string =
    string
        |> Helpers.stringToDecimal
        |> Maybe.map fromDecimal


fromDecimal : Decimal -> Judgment
fromDecimal decimal =
    decimal
        |> Decimal.mul onePercent
        |> Judgment


fromFloat : Float -> Maybe Judgment
fromFloat float =
    float
        |> Decimal.fromFloat
        |> Maybe.map fromDecimal


toString : Judgment -> String
toString rate_ =
    rate_
        |> toDecimal
        |> Decimal.toFloat
        |> String.fromFloat


toDecimal : Judgment -> Decimal
toDecimal (Judgment decimal) =
    decimal
        |> Decimal.mul (Decimal.fromInt 100)


onePercent : Decimal
onePercent =
    toPercent 1


toPercent : Int -> Decimal
toPercent int =
    Decimal.fromIntWithExponent int -2
