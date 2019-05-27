module Interest exposing
    ( Interest
    , calculate
    , calculateFromStrings
    , dailyFactor
    , format
    , toDecimal
    , toFloat
    , toString
    )

import Date exposing (Date)
import Decimal exposing (Decimal)
import Helpers
import Judgment exposing (..)
import Rate exposing (Rate)
import Time exposing (Month(..))


type Interest
    = Interest Decimal


fromDecimal : Decimal -> Interest
fromDecimal decimal =
    Interest decimal


toDecimal : Interest -> Decimal
toDecimal (Interest decimal) =
    decimal


format : Interest -> String
format (Interest decimal) =
    decimal
        |> Helpers.formatDecimal


toFloat : Interest -> Float
toFloat (Interest decimal) =
    decimal
        |> Decimal.toFloat


toString : Interest -> String
toString (Interest decimal) =
    decimal
        |> Decimal.toString


dailyFactor : Decimal
dailyFactor =
    Decimal.fromIntWithExponent 2739726 -9


diff : Date -> Date -> Int
diff date1 date2 =
    date1
        |> Date.diff Date.Days date2
        |> abs


calculate : Judgment -> Rate -> Date -> Date -> Interest
calculate judgment rate date1 date2 =
    let
        days =
            diff date1 date2
    in
    Judgment.toDecimal judgment
        |> Decimal.mul (Rate.toDecimal rate)
        |> Decimal.mul (Decimal.fromInt days)
        |> Decimal.mul dailyFactor
        |> Decimal.round -2
        |> Interest


calculateFromStrings : String -> String -> String -> String -> Interest
calculateFromStrings judgment rate date1 date2 =
    let
        j =
            judgment
                |> Judgment.fromString
                |> Maybe.withDefault Judgment.zero

        r =
            rate
                |> Rate.fromString
                |> Maybe.withDefault Rate.zero

        d1 =
            date1
                |> Date.fromIsoString
                |> Result.withDefault (Date.fromRataDie 0)

        d2 =
            date2
                |> Date.fromIsoString
                |> Result.withDefault d1
    in
    calculate j r d1 d2
