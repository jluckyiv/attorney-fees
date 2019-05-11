module Main exposing (Model, Msg(..), init, main, update, view)

import AttorneyFees
import Browser
import Browser.Dom as Dom
import Bulma.Classes as Bu
import FontAwesome as Fa
import Html exposing (Html, button, div, form, h1, h2, h3, input, label, p, section, span, text)
import Html.Attributes exposing (class, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Money
import Task



---- MODEL ----


type alias Model =
    { current : String
    , previous : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" "", focus )



---- UPDATE ----


type Msg
    = Ignored
    | Clear
    | UpdatedJudgmentAmount String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignored ->
            ( model, Cmd.none )

        Clear ->
            ( clear model, focus )

        UpdatedJudgmentAmount string ->
            ( { model | current = string }, Cmd.none )


focus : Cmd Msg
focus =
    Task.attempt (\_ -> Ignored) (Dom.focus "input")


clear : Model -> Model
clear model =
    if model.current == "" then
        model

    else
        { model | previous = model.current, current = "" }



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        judgment =
            if model.current == "" then
                model.previous

            else
                model.current

        judgmentMoney =
            judgment
                |> String.replace "," ""
                |> String.replace "$" ""
                |> String.toFloat
                |> Maybe.withDefault 0
                |> Money.fromFloat

        fees =
            judgmentMoney
                |> AttorneyFees.fromJudgmentAmount
    in
    div []
        [ section [ class Bu.hero, class Bu.isInfo ]
            [ div [ class Bu.heroBody ]
                [ div [ class Bu.container ]
                    [ h1 [ class Bu.title ] [ text "Default Judgment Attorney Fees" ]
                    , h2 [ class Bu.subtitle ] [ text "Calculated per Riverside Superior Court Local Rule 3190 (Rev. 1-1-12)" ]
                    ]
                ]
            ]
        , section [ class Bu.section ]
            [ div [ class Bu.container ]
                [ form [ onSubmit Clear ]
                    [ div
                        [ class Bu.field
                        , class Bu.hasAddons
                        ]
                        [ div
                            [ class Bu.control
                            , class Bu.hasIconsLeft
                            ]
                            [ input
                                [ id "input"
                                , type_ "number"
                                , class Bu.input
                                , onInput UpdatedJudgmentAmount
                                , placeholder "Judgment amount"
                                , value model.current
                                ]
                                []
                            , span [ class Bu.icon, class Bu.isSmall, class Bu.isLeft ]
                                [ Fa.icon Fa.dollarSign ]
                            ]
                        , div [ class Bu.control ]
                            [ button [ class Bu.isLink, class Bu.button, onClick Clear ] [ text "Clear" ] ]
                        ]
                    , if model.current == "" then
                        p [ class Bu.isLink, class Bu.help ] [ text "Fees will update automatically" ]

                      else
                        p [ class Bu.isLink, class Bu.help ] [ text "Press [Enter/Return] to clear" ]
                    ]
                ]
            , div [ class Bu.container ]
                [ h2 [ class Bu.subtitle ]
                    [ text
                        ("$"
                            ++ Money.format judgmentMoney
                            ++ " judgment = $"
                            ++ Money.format fees
                            ++ " fees."
                        )
                    ]
                ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
