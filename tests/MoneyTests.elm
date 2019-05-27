module MoneyTests exposing (all)

import Decimal
import Expect
import Fuzz
import Money
import Test exposing (..)


all : Test
all =
    describe "Money"
        [ describe "Creating"
            [ test "fromDecimal" <|
                \_ ->
                    Decimal.fromIntWithExponent 123456789 -3
                        |> Money.fromDecimal
                        |> Money.toString
                        |> Expect.equal "123,456.79"
            , test "fromInt" <|
                \_ ->
                    123456
                        |> Money.fromInt
                        |> Money.toString
                        |> Expect.equal "123,456.00"
            , test "fromFloat" <|
                \_ ->
                    123456.789
                        |> Money.fromFloat
                        |> Maybe.map Money.toString
                        |> Expect.equal (Just "123,456.79")
            , test "fromString" <|
                \_ ->
                    "$ 123,456.789"
                        |> Money.fromString
                        |> Maybe.map Money.toString
                        |> Expect.equal (Just "123,456.79")
            ]
        , describe "Formatting"
            [ test "format" <|
                \_ ->
                    123456789
                        |> Money.fromInt
                        |> Money.format
                        |> Expect.equal "123,456,789.00"
            ]
        ]
