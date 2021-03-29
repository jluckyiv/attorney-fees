module Money exposing
    ( Money
    , format
    , fromDecimal
    , fromFloat
    , fromInt
    , fromString
    , toString
    )

import Decimal exposing (Decimal)
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Helpers


type Money
    = Money Decimal


fromString : String -> Maybe Money
fromString string =
    string
        |> Helpers.stringToDecimal
        |> Maybe.map Money


fromInt : Int -> Money
fromInt int =
    int
        |> Decimal.fromInt
        |> Money


fromDecimal : Decimal -> Money
fromDecimal decimal =
    decimal
        |> Money


fromFloat : Float -> Maybe Money
fromFloat float =
    float
        |> Decimal.fromFloat
        |> Maybe.map Money


toString : Money -> String
toString (Money decimal) =
    decimal
        |> Decimal.toFloat
        |> FormatNumber.format usLocale


format : Money -> String
format money =
    money
        |> toString
