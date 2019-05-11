module AttorneyFees exposing (fromJudgmentAmount)

import Money exposing (..)


fromJudgmentAmount : Money -> Money
fromJudgmentAmount money =
    let
        dollars =
            toDollars money

        level1 =
            1000

        level2 =
            7500

        level3 =
            15000

        level4 =
            25000

        calculate previousLevel rate money_ =
            money_
                |> Money.add (Money.fromDollars <| negate previousLevel)
                |> Money.multiply (Decimal rate)
                |> Money.add (fromJudgmentAmount (Money.fromDollars previousLevel))
    in
    if dollars <= level1 then
        money |> Money.multiply (Decimal 0.25)

    else if dollars <= level2 then
        money |> calculate level1 0.15

    else if dollars <= level3 then
        money |> calculate level2 0.1

    else if dollars <= level4 then
        money |> calculate level3 0.04

    else
        money
            |> calculate level4 0.02
            -- adjust for math error in Rule 3190
            |> Money.add (Money.fromDollars -100)
