module HelpersTests exposing (all)

import Decimal exposing (Decimal)
import Expect
import Fuzz exposing (..)
import Helpers
import Test exposing (..)


all : Test
all =
    describe "Helper"
        [ describe "stringToFloat"
            [ test "with commas" <|
                \_ ->
                    "1,234,567,890"
                        |> Helpers.stringToFloat
                        |> Expect.equal (Just 1234567890)
            , test "with commas and decimal" <|
                \_ ->
                    "1,234,567.890"
                        |> Helpers.stringToFloat
                        |> Expect.equal (Just 1234567.89)
            , test "with dollar sign, space, commas, and decimal" <|
                \_ ->
                    "$ 1,234,567.890"
                        |> Helpers.stringToFloat
                        |> Expect.equal (Just 1234567.89)
            ]
        , describe "check my decimal assumptions"
            [ test "Decimal.fromFloat and Decimal.fromIntWithExponent" <|
                \_ ->
                    Decimal.fromFloat 0.123456789
                        |> Expect.equal (Just <| Decimal.fromIntWithExponent 123456789 -9)
            ]
        , describe "stringToDecimal"
            [ test "with commas" <|
                \_ ->
                    "1,234,567,890"
                        |> Helpers.stringToDecimal
                        |> Expect.equal (Just <| Decimal.fromInt 1234567890)
            , test "with commas and decimal" <|
                \_ ->
                    "1,234,567.890"
                        |> Helpers.stringToDecimal
                        |> Expect.equal (Decimal.fromFloat 1234567.89)
            , test "with dollar sign, space, commas, and decimal" <|
                \_ ->
                    "$ 1,234,567.890"
                        |> Helpers.stringToDecimal
                        |> Expect.equal (Decimal.fromFloat 1234567.89)
            ]
        , test "formatString" <|
            \_ ->
                "123456.789"
                    |> Helpers.formatString
                    |> Expect.equal "123,456.79"
        , test "formatFloat" <|
            \_ ->
                123456.789
                    |> Helpers.formatFloat
                    |> Expect.equal "123,456.79"
        , test "formatDecimal" <|
            \_ ->
                Decimal.fromIntWithExponent 1234567890 -4
                    |> Helpers.formatDecimal
                    |> Expect.equal "123,456.79"
        ]
