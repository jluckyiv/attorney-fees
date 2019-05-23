module Tests exposing (all)

import AttorneyFees
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (..)
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Calculate attorney fees under Riverside Local Rule 3190"
        [ describe "AttorneyFees"
            [ test "The total judgment is 25% of $1" <|
                \_ ->
                    "0"
                        |> AttorneyFees.fromJudgmentAmount "1"
                        |> Expect.equal "0.25"
            , test "The total judgment is 25% of $1,000" <|
                \_ ->
                    "0"
                        |> AttorneyFees.fromJudgmentAmount "1000"
                        |> Expect.equal "250.00"
            , fuzz (intRange 1 1000) "If the total judgment is up to $1,000" <|
                \dollars ->
                    let
                        judgmentAmount =
                            String.fromInt dollars
                    in
                    "0"
                        |> AttorneyFees.fromJudgmentAmount judgmentAmount
                        |> String.toFloat
                        |> Maybe.withDefault 0
                        |> Expect.within
                            (Absolute 0.000000001)
                            (0.0 + toFloat dollars * 0.25)
            , test "The total judgment is $250 + 15% of $1,001" <|
                \_ ->
                    "0"
                        |> AttorneyFees.fromJudgmentAmount "1001"
                        |> Expect.equal "250.15"
            , test "The total judgment is $250 + 15% of $7,500" <|
                \_ ->
                    "0"
                        |> AttorneyFees.fromJudgmentAmount "7500"
                        |> Expect.equal "1225.00"
            , fuzz (intRange 1001 7500) "If the total judgment is up to $7,500" <|
                \dollars ->
                    let
                        judgmentAmount =
                            String.fromInt dollars
                    in
                    "0"
                        |> AttorneyFees.fromJudgmentAmount judgmentAmount
                        |> String.toFloat
                        |> Maybe.withDefault 0
                        |> Expect.within
                            (Absolute 0.000000001)
                            (250.0 + toFloat (dollars - 1000) * 0.15)
            , test "The total judgment is $1,225 + 10% of $7,501" <|
                \_ ->
                    "0"
                        |> AttorneyFees.fromJudgmentAmount "7501"
                        |> Expect.equal "1225.10"
            , fuzz (intRange 7501 15000) "If the total judgment is up to $15,000" <|
                \dollars ->
                    let
                        judgmentAmount =
                            String.fromInt dollars
                    in
                    "0"
                        |> AttorneyFees.fromJudgmentAmount judgmentAmount
                        |> String.toFloat
                        |> Maybe.withDefault 0
                        |> Expect.within
                            (Absolute 0.000000001)
                            (1225.0 + toFloat (dollars - 7500) * 0.1)
            , test "The total judgment is $1,975 + 4% of $15,001" <|
                \_ ->
                    "0"
                        |> AttorneyFees.fromJudgmentAmount "15001"
                        |> Expect.equal "1975.04"
            , test "The total judgment is $1,975 + 4% of $25,000" <|
                \_ ->
                    "0"
                        |> AttorneyFees.fromJudgmentAmount "25000"
                        |> Expect.equal "2375.00"
            , fuzz (intRange 15001 25000) "If the total judgment is up to $25,000" <|
                \dollars ->
                    let
                        judgmentAmount =
                            String.fromInt dollars
                    in
                    "0"
                        |> AttorneyFees.fromJudgmentAmount judgmentAmount
                        |> String.toFloat
                        |> Maybe.withDefault 0
                        |> Expect.within
                            (Absolute 0.000000001)
                            (1975.0 + toFloat (dollars - 15000) * 0.04)
            , test "The total judgment is $2,275 + 2% of $30,000" <|
                \_ ->
                    "0"
                        |> AttorneyFees.fromJudgmentAmount "30000"
                        |> Expect.equal "2375.00"
            , test "The total judgment is $2,275 + 2% of $25,001" <|
                \_ ->
                    "0"
                        |> AttorneyFees.fromJudgmentAmount "25001"
                        |> Expect.equal "2275.02"
            , fuzz (intRange 25001 100000000) "If the total judgment is over $25,000" <|
                \dollars ->
                    let
                        judgmentAmount =
                            String.fromInt dollars
                    in
                    "0"
                        |> AttorneyFees.fromJudgmentAmount judgmentAmount
                        |> String.toFloat
                        |> Maybe.withDefault 0
                        |> Expect.within
                            (Absolute 0.000000001)
                            (2275.0 + toFloat (dollars - 25000) * 0.02)
            ]
        ]
