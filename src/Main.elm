module Main exposing (Model, Msg(..), init, main, update, view)

import AttorneyFees
import Browser
import Browser.Dom as Dom
import Bulma.Classes as Bu
import FontAwesome as Fa
import Html exposing (Html, button, div, form, h1, h2, h3, input, label, p, section, span, text)
import Html.Attributes exposing (class, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Money exposing (Money)
import Task



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



---- MODEL ----


type Model
    = Current Amount
    | Cached Amount


type alias Amount =
    String


amount : Model -> Amount
amount model =
    case model of
        Current a ->
            a

        Cached a ->
            a


init : ( Model, Cmd Msg )
init =
    ( Current "", focus inputId )


inputId : String
inputId =
    "input"



---- UPDATE ----


type Msg
    = Ignored
    | Clear
    | UpdatedAmount String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignored ->
            ( model, Cmd.none )

        Clear ->
            ( clear model, focus inputId )

        UpdatedAmount string ->
            ( Current string, Cmd.none )


focus : String -> Cmd Msg
focus elementId =
    Task.attempt (\_ -> Ignored) (Dom.focus elementId)


clear : Model -> Model
clear model =
    case model of
        Current a ->
            Cached a

        Cached a ->
            Current ""



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ section [ class Bu.hero, class Bu.isInfo ]
            [ viewHero ]
        , section [ class Bu.section ]
            [ viewForm model
            , viewCalculation (amount model)
            ]
        ]


viewHero : Html msg
viewHero =
    div [ class Bu.heroBody ]
        [ div [ class Bu.container ]
            [ h1 [ class Bu.title ]
                [ text "Default Judgment Attorney Fees" ]
            , h2 [ class Bu.subtitle ]
                [ text "Calculated per Riverside Superior Court Local Rule 3190 (Rev. 1-1-12)" ]
            ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    div [ class Bu.container ]
        [ form [ onSubmit Clear ]
            [ viewFormBody model
            , viewFormHelp model
            ]
        ]


viewFormBody : Model -> Html Msg
viewFormBody model =
    div [ class Bu.field, class Bu.hasAddons ]
        [ viewInput model
        , viewClearButton
        ]


viewInput : Model -> Html Msg
viewInput model =
    let
        value_ =
            case model of
                Current a ->
                    a

                Cached _ ->
                    ""
    in
    div [ class Bu.control, class Bu.hasIconsLeft ]
        [ input
            [ id "input"
            , type_ "number"
            , class Bu.input
            , onInput UpdatedAmount
            , placeholder "Judgment amount"
            , value value_
            ]
            []
        , iconDollarSign
        ]


iconDollarSign : Html msg
iconDollarSign =
    span [ class Bu.icon, class Bu.isSmall, class Bu.isLeft ]
        [ Fa.icon Fa.dollarSign ]


viewClearButton : Html Msg
viewClearButton =
    div [ class Bu.control ]
        [ button [ class Bu.isLink, class Bu.button ] [ text "Clear" ] ]


viewFormHelp : Model -> Html msg
viewFormHelp model =
    case model of
        Cached _ ->
            p [ class Bu.isLink, class Bu.help ] [ text "Fees will update automatically" ]

        Current _ ->
            p [ class Bu.isLink, class Bu.help ] [ text "Press [Enter/Return] to clear" ]


viewCalculation : Amount -> Html msg
viewCalculation amount_ =
    let
        judgment =
            amount_
                |> String.replace "," ""
                |> String.replace "$" ""
                |> String.toFloat
                |> Maybe.withDefault 0
                |> Money.fromFloat

        fees =
            judgment
                |> AttorneyFees.fromJudgmentAmount
    in
    div [ class Bu.container ]
        [ h2 [ class Bu.subtitle ]
            [ text
                ("$"
                    ++ Money.format judgment
                    ++ " judgment = $"
                    ++ Money.format fees
                    ++ " fees."
                )
            ]
        ]
