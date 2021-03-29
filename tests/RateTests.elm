module RateTests exposing (all)

import Decimal exposing (Decimal)
import Expect
import Fuzz exposing (..)
import Rate
import Test exposing (..)


all : Test
all =
    describe "Rate"
        [ test "fromString" <|
            \_ ->
                "12.75%"
                    |> Rate.fromString
                    |> Maybe.map Rate.toString
                    |> Expect.equal (Just "12.75")
        , test "fromFloat" <|
            \_ ->
                12.75
                    |> Rate.fromFloat
                    |> Maybe.map Rate.toString
                    |> Expect.equal (Just "12.75")
        , test "fromDecimal" <|
            \_ ->
                Decimal.fromIntWithExponent 1275 -2
                    |> Rate.fromDecimal
                    |> Rate.toString
                    |> Expect.equal "12.75"
        , test "toDecimal" <|
            \_ ->
                Decimal.fromIntWithExponent 1275 -2
                    |> Rate.fromDecimal
                    |> Rate.toDecimal
                    |> Expect.equal (Decimal.fromIntWithExponent 1275 -4)
        ]
