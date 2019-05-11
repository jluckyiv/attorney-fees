module Money exposing
    ( Money(..)
    , Number(..)
    , add
    , divide
    , format
    , fromCents
    , fromDollars
    , fromFloat
    , multiply
    , subtract
    , sum
    , toCents
    , toDollars
    , toString
    , zero
    )

{- Code derived from dinero.js -}

import FormatNumber
import FormatNumber.Locales exposing (usLocale)


type Money
    = Money Int


type Number
    = Whole Int
    | Decimal Float


fromCents : Int -> Money
fromCents amount =
    Money amount


fromDollars : Int -> Money
fromDollars int =
    Money (int * 100)


fromFloat : Float -> Money
fromFloat float =
    float
        |> (*) 100
        |> round
        |> fromCents


zero : Money
zero =
    fromCents 0


toCents : Money -> Int
toCents (Money amount) =
    amount


toDollars : Money -> Float
toDollars cent =
    cent
        |> toCents
        |> toFloat
        |> (*) 0.01


toString : Money -> String
toString cent =
    cent
        |> toDollars
        |> String.fromFloat


format : Money -> String
format cent =
    let
        locale =
            { usLocale | decimals = 2 }
    in
    cent
        |> toDollars
        |> FormatNumber.format locale


add : Money -> Money -> Money
add (Money a) (Money b) =
    Money (a + b)


sum : List Money -> Money
sum cents =
    cents
        |> List.foldl add (Money 0)


subtract : Money -> Money -> Money
subtract (Money a) (Money b) =
    Money (a - b)


multiply : Number -> Money -> Money
multiply number (Money amount) =
    case number of
        Whole int ->
            Money (amount * int)

        Decimal float ->
            Money (floatMultiply float amount)


floatMultiply : Float -> Int -> Int
floatMultiply a b =
    let
        floatB =
            toFloat b

        countFractionDigits float =
            float
                |> String.fromFloat
                |> String.split "."
                |> List.drop 1
                |> List.head
                |> Maybe.map String.length
                |> Maybe.withDefault 0

        getFactor number =
            10 ^ countFractionDigits number

        factor =
            max (getFactor a) (getFactor floatB) |> toFloat
    in
    round (toFloat (round (a * factor) * round (floatB * factor)) / (factor * factor))


divide : Float -> Money -> Money
divide float (Money amount) =
    Money (toFloat amount / float |> round)
