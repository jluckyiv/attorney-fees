module Helpers exposing
    ( formatDecimal
    , formatFloat
    , formatString
    , stringToDecimal
    , stringToFloat
    )

import Decimal exposing (Decimal)
import FormatNumber
import FormatNumber.Locales exposing (usLocale)


formatString : String -> String
formatString string =
    string
        |> stringToFloat
        |> Maybe.map formatFloat
        |> Maybe.withDefault string


formatDecimal : Decimal -> String
formatDecimal decimal =
    decimal
        |> Decimal.toFloat
        |> formatFloat


formatFloat : Float -> String
formatFloat float =
    float
        |> FormatNumber.format usLocale


stringToDecimal : String -> Maybe Decimal
stringToDecimal string =
    string
        |> stringToFloat
        |> Maybe.andThen Decimal.fromFloat


stringToFloat : String -> Maybe Float
stringToFloat string =
    string
        |> String.filter isFloaty
        |> String.toFloat


isFloaty : Char -> Bool
isFloaty char =
    Char.isDigit char || '.' == char
