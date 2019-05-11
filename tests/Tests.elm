module Tests exposing (all)

import AttorneyFees
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (..)
import Money
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Calculate attorney fees under Riverside Local Rule 3190"
        [ describe "Money"
            [ test "Add cents" <|
                \_ ->
                    Money.fromCents 100
                        |> Money.add (Money.fromCents 100)
                        |> Money.toCents
                        |> Expect.equal 200
            , test "Add dollars" <|
                \_ ->
                    Money.fromDollars 1
                        |> Money.add (Money.fromDollars 1)
                        |> Money.toCents
                        |> Expect.equal 200
            , test "Sum" <|
                \_ ->
                    [ Money.fromCents 101, Money.fromCents 101 ]
                        |> Money.sum
                        |> Money.toCents
                        |> Expect.equal 202
            , test "Subtract" <|
                \_ ->
                    Money.fromCents 100
                        |> Money.subtract (Money.fromCents 100)
                        |> Money.toCents
                        |> Expect.equal 0
            , test "Multiply with Int" <|
                \_ ->
                    Money.fromCents 100
                        |> Money.multiply (Money.Whole 100)
                        |> Money.toCents
                        |> Expect.equal 10000
            , test "Multiply with Float" <|
                \_ ->
                    Money.fromCents 10000
                        |> Money.multiply (Money.Decimal 0.04)
                        |> Money.toCents
                        |> Expect.equal 400
            , test "Multiply with large values with Float" <|
                \_ ->
                    Money.fromCents 34312365789
                        |> Money.multiply (Money.Decimal 0.0000125)
                        |> Money.toCents
                        |> Expect.equal 428905
            , test "Divide with large values" <|
                \_ ->
                    Money.fromCents 34312365789
                        |> Money.divide 5128515
                        |> Money.toDollars
                        |> Expect.within (Absolute 0.000000001) 66.91
            , test "To string" <|
                \_ ->
                    Money.fromCents 12345678
                        |> Money.toString
                        |> Expect.equal "123456.78"
            , test "Format" <|
                \_ ->
                    Money.fromCents 123456789
                        |> Money.format
                        |> Expect.equal "1,234,567.89"
            ]
        , describe "AttorneyFees"
            [ test "The total judgment is 25% of $1" <|
                \_ ->
                    Money.fromDollars 1
                        |> AttorneyFees.fromJudgmentAmount
                        |> Money.format
                        |> Expect.equal "0.25"
            , test "The total judgment is 25% of $1,000" <|
                \_ ->
                    Money.fromDollars 1000
                        |> AttorneyFees.fromJudgmentAmount
                        |> Money.format
                        |> Expect.equal "250.00"
            , fuzz (intRange 1 1000) "If the total judgment is up to $1,000" <|
                \dollars ->
                    let
                        judgmentAmount =
                            Money.fromDollars dollars
                    in
                    judgmentAmount
                        |> AttorneyFees.fromJudgmentAmount
                        |> Money.toDollars
                        |> Expect.within
                            (Absolute 0.000000001)
                            (0.0 + toFloat dollars * 0.25)
            , test "The total judgment is $250 + 15% of $1,001" <|
                \_ ->
                    Money.fromDollars 1001
                        |> AttorneyFees.fromJudgmentAmount
                        |> Money.format
                        |> Expect.equal "250.15"
            , test "The total judgment is $250 + 15% of $7,500" <|
                \_ ->
                    Money.fromDollars 7500
                        |> AttorneyFees.fromJudgmentAmount
                        |> Money.format
                        |> Expect.equal "1,225.00"
            , fuzz (intRange 1001 7500) "If the total judgment is up to $7,500" <|
                \dollars ->
                    let
                        judgmentAmount =
                            Money.fromDollars dollars
                    in
                    judgmentAmount
                        |> AttorneyFees.fromJudgmentAmount
                        |> Money.toDollars
                        |> Expect.within
                            (Absolute 0.000000001)
                            (250.0 + toFloat (dollars - 1000) * 0.15)
            , test "The total judgment is $1,225 + 10% of $7,501" <|
                \_ ->
                    Money.fromDollars 7501
                        |> AttorneyFees.fromJudgmentAmount
                        |> Money.format
                        |> Expect.equal "1,225.10"
            , fuzz (intRange 7501 15000) "If the total judgment is up to $15,000" <|
                \dollars ->
                    let
                        judgmentAmount =
                            Money.fromDollars dollars
                    in
                    judgmentAmount
                        |> AttorneyFees.fromJudgmentAmount
                        |> Money.toDollars
                        |> Expect.within
                            (Absolute 0.000000001)
                            (1225.0 + toFloat (dollars - 7500) * 0.1)
            , test "The total judgment is $1,975 + 4% of $15,001" <|
                \_ ->
                    Money.fromDollars 15001
                        |> AttorneyFees.fromJudgmentAmount
                        |> Money.format
                        |> Expect.equal "1,975.04"
            , test "The total judgment is $1,975 + 4% of $25,000" <|
                \_ ->
                    Money.fromDollars 25000
                        |> AttorneyFees.fromJudgmentAmount
                        |> Money.format
                        |> Expect.equal "2,375.00"
            , fuzz (intRange 15001 25000) "If the total judgment is up to $25,000" <|
                \dollars ->
                    let
                        judgmentAmount =
                            Money.fromDollars dollars
                    in
                    judgmentAmount
                        |> AttorneyFees.fromJudgmentAmount
                        |> Money.toDollars
                        |> Expect.within
                            (Absolute 0.000000001)
                            (1975.0 + toFloat (dollars - 15000) * 0.04)
            , test "The total judgment is $2,275 + 2% of $30,000" <|
                \_ ->
                    Money.fromDollars 30000
                        |> AttorneyFees.fromJudgmentAmount
                        |> Money.format
                        |> Expect.equal "2,375.00"
            , test "The total judgment is $2,275 + 2% of $25,001" <|
                \_ ->
                    Money.fromDollars 25001
                        |> AttorneyFees.fromJudgmentAmount
                        |> Money.format
                        |> Expect.equal "2,275.02"

            -- , fuzz (intRange 25001 100000000) "If the total judgment is over $25,000" <|
            --     \dollars ->
            --         let
            --             judgmentAmount =
            --                 Money.fromDollars dollars
            --         in
            --         judgmentAmount
            --             |> AttorneyFees.fromJudgmentAmount
            --             |> Money.toDollars
            --             |> Expect.within
            --                 (Absolute 0.000000001)
            --                 (2275.0 + toFloat (dollars - 25000) * 0.02)
            ]
        ]
