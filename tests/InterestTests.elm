module InterestTests exposing (all)

import Date
import Decimal
import Expect
import Helpers
import Interest
import Judgment
import Rate
import Test exposing (..)
import Time exposing (Month(..))


all : Test
all =
    describe "Interest"
        [ test "calculate" <|
            \_ ->
                let
                    judgment =
                        Judgment.fromFloat 20000.0
                            |> Maybe.withDefault Judgment.zero

                    rate =
                        Rate.fromFloat 10.0
                            |> Maybe.withDefault Rate.zero

                    from =
                        Date.fromCalendarDate 2018 Jan 1

                    to =
                        Date.fromCalendarDate 2019 Jan 1
                in
                Interest.calculate judgment rate from to
                    |> Interest.toDecimal
                    |> Expect.equal (Decimal.fromIntWithExponent 200000 -2)
        , test "toString" <|
            \_ ->
                let
                    judgment =
                        Judgment.fromFloat 20000.0
                            |> Maybe.withDefault Judgment.zero

                    rate =
                        Rate.fromFloat 10.0
                            |> Maybe.withDefault Rate.zero

                    from =
                        Date.fromCalendarDate 2018 Jan 1

                    to =
                        Date.fromCalendarDate 2019 Jan 1
                in
                Interest.calculate judgment rate from to
                    |> Interest.toString
                    |> Expect.equal "2000.00"
        , test "format" <|
            \_ ->
                let
                    judgment =
                        Judgment.fromFloat 20000.0
                            |> Maybe.withDefault Judgment.zero

                    rate =
                        Rate.fromFloat 10.0
                            |> Maybe.withDefault Rate.zero

                    from =
                        Date.fromCalendarDate 2018 Jan 1

                    to =
                        Date.fromCalendarDate 2019 Jan 1
                in
                Interest.calculate judgment rate from to
                    |> Interest.format
                    |> Expect.equal "2,000.00"
        ]
