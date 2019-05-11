module Main exposing (Model, Msg(..), init, main, update, view)

import AttorneyFees
import Browser
import Browser.Dom as Dom
import Html exposing (Html, button, div, form, h1, h2, h3, input, p, span, text)
import Html.Attributes exposing (id, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Money
import Task



---- MODEL ----


type alias Model =
    { judgmentAmount : String }


init : ( Model, Cmd Msg )
init =
    ( Model "", focus )



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
            ( { model | judgmentAmount = "" }, focus )

        UpdatedJudgmentAmount string ->
            ( { model | judgmentAmount = string }, Cmd.none )


focus : Cmd Msg
focus =
    Task.attempt (\_ -> Ignored) (Dom.focus "input")



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        value_ =
            model.judgmentAmount

        fees =
            value_
                |> String.replace "," ""
                |> String.replace "$" ""
                |> String.toFloat
                |> Maybe.withDefault 0
                |> Money.fromFloat
                |> AttorneyFees.fromJudgmentAmount
    in
    div []
        [ h1 [] [ text "Default Judgment Attorney Fees" ]
        , p [] [ text "Calculated per Riverside Superior Court Local Rule 3190 (Rev. 1-1-12)" ]
        , form [ onSubmit Clear ]
            [ span [] [ text "$ " ]
            , input [ id "input", onInput UpdatedJudgmentAmount, value value_ ] []
            , button [ onClick Clear ] [ text "Clear" ]
            ]
        , h3 [] [ text "Press [Enter] to clear" ]
        , h2 [] [ text ("Attorney fees = $" ++ Money.format fees) ]
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
