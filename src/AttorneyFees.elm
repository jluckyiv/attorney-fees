module AttorneyFees exposing (fromJudgmentAmount)

import Decimal exposing (Decimal)


fromJudgmentAmount : String -> String -> String
fromJudgmentAmount judgment interest =
    let
        isFloaty c =
            Char.isDigit c || c == '.'

        toDecimal str =
            str
                |> String.filter isFloaty
                |> Decimal.fromString
                |> Maybe.withDefault (Decimal.fromInt 0)

        total =
            Decimal.add (toDecimal judgment) (toDecimal interest)
    in
    calculateJudgment total
        |> Decimal.toString


calculateJudgment : Decimal -> Decimal
calculateJudgment amount =
    let
        level1 : Decimal
        level1 =
            Decimal.fromInt 1000

        level2 : Decimal
        level2 =
            Decimal.fromInt 7500

        level3 : Decimal
        level3 =
            Decimal.fromInt 15000

        level4 : Decimal
        level4 =
            Decimal.fromInt 25000

        calculate level rate amount_ =
            Decimal.sub amount_ level
                |> Decimal.mul rate
                |> Decimal.add (calculateJudgment level)
    in
    if Decimal.gt amount level4 then
        amount
            |> calculate level4 (Decimal.fromIntWithExponent 2 -2)

    else if Decimal.gt amount level3 then
        amount
            |> calculate level3 (Decimal.fromIntWithExponent 4 -2)

    else if Decimal.gt amount level2 then
        amount
            |> calculate level2 (Decimal.fromIntWithExponent 10 -2)

    else if Decimal.gt amount level1 then
        amount
            |> calculate level1 (Decimal.fromIntWithExponent 15 -2)

    else
        amount
            |> Decimal.mul (Decimal.fromIntWithExponent 25 -2)
