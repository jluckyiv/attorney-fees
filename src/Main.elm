module Main exposing (main)

import AttorneyFees
import Browser
import Browser.Dom as Dom
import Bulma.Classes as Bu
import FontAwesome as Fa
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Html exposing (Html, a, button, div, form, h1, h2, input, label, p, section, span, text)
import Html.Attributes exposing (class, href, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Task



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



---- MODEL ----


type alias Flags =
    { name : String, email : String }


type alias Model =
    { contact : Flags
    , data : Data
    }


type Data
    = Current Amount Interest
    | Cached Amount Interest


type alias Amount =
    String


type alias Interest =
    String


type alias ViewInputParams =
    { toMsg : String -> Msg
    , inputId : String
    , inputValue : String
    , inputPlaceholder : String
    , inputLabel : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { contact = flags, data = Current "" "" }, focus judgmentInputId )


judgmentInputId : String
judgmentInputId =
    "judgmentInput"


interestInputId : String
interestInputId =
    "interestInput"



---- UPDATE ----


type Msg
    = Ignored
    | Clear
    | UpdatedJudgmentInput String
    | UpdatedInterestInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignored ->
            ( model, Cmd.none )

        Clear ->
            ( clear model, focus judgmentInputId )

        UpdatedJudgmentInput string ->
            ( updateJudgmentAmount model string, Cmd.none )

        UpdatedInterestInput string ->
            ( updateInterestAmount model string, Cmd.none )


updateJudgmentAmount : Model -> String -> Model
updateJudgmentAmount model string =
    case model.data of
        Current _ _ ->
            { model | data = Current string (interest model) }

        Cached _ _ ->
            { model | data = Current string "" }


updateInterestAmount : Model -> String -> Model
updateInterestAmount model string =
    { model | data = Current (amount model) string }



---- HELPERS ----


amount : Model -> Amount
amount model =
    case model.data of
        Current a _ ->
            a

        Cached a _ ->
            a


interest : Model -> Interest
interest model =
    case model.data of
        Current _ i ->
            i

        Cached _ i ->
            i


clear : Model -> Model
clear model =
    case model.data of
        Current a i ->
            { model | data = Cached a i }

        Cached _ _ ->
            { model | data = Current "" "" }


focus : String -> Cmd Msg
focus elementId =
    Task.attempt (\_ -> Ignored) (Dom.focus elementId)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewHero
        , viewBody model
        ]


viewHero : Html msg
viewHero =
    let
        h1Text =
            "Default Judgment Attorney Fees"

        h2Text =
            "Calculated per Riverside Superior Court Local Rule 3190 (Rev. 1-1-12)"
    in
    section [ class Bu.hero, class Bu.isInfo ]
        [ div [ class Bu.heroBody ]
            [ div [ class Bu.container ]
                [ h1 [ class Bu.title ] [ text h1Text ]
                , h2 [ class Bu.subtitle ] [ text h2Text ]
                ]
            ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    section [ class Bu.section ]
        [ div [ class Bu.container ]
            [ div [ class Bu.columns ]
                [ viewForm model
                , viewCalculation model
                ]
            , div [ class Bu.columns ] [ viewSuggestions model ]
            ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    div
        [ class Bu.column
        , class Bu.is5Tablet
        , class Bu.is4Desktop
        , class Bu.is3Widescreen
        ]
        [ form
            [ onSubmit Clear
            , class Bu.box
            ]
            [ viewFormBody model
            ]
        ]


viewFormBody : Model -> Html Msg
viewFormBody model =
    div
        []
        [ viewJudgmentInput model
        , viewInterestInput model
        , viewFormHelp model
        , viewClearButton
        ]


viewJudgmentInput : Model -> Html Msg
viewJudgmentInput model =
    let
        inputValue =
            case model.data of
                Current a _ ->
                    a

                Cached _ _ ->
                    ""

        inputLabel =
            "Judgment"

        inputPlaceholder =
            "Judgment amount"

        toMsg =
            UpdatedJudgmentInput
    in
    viewInput (ViewInputParams toMsg judgmentInputId inputValue inputPlaceholder inputLabel)


viewInterestInput : Model -> Html Msg
viewInterestInput model =
    let
        inputValue =
            case model.data of
                Current _ i ->
                    i

                Cached _ _ ->
                    ""

        inputLabel =
            "Interest"

        inputPlaceholder =
            "Interest amount"

        toMsg =
            UpdatedInterestInput
    in
    viewInput (ViewInputParams toMsg interestInputId inputValue inputPlaceholder inputLabel)


viewInput : ViewInputParams -> Html Msg
viewInput { toMsg, inputId, inputValue, inputPlaceholder, inputLabel } =
    let
        inputType =
            "text"
    in
    div [ class Bu.field ]
        [ label [ class Bu.label ] [ text inputLabel ]
        , div [ class Bu.control, class Bu.hasIconsLeft ]
            [ input
                [ id inputId
                , type_ inputType
                , placeholder inputPlaceholder
                , value inputValue
                , class Bu.input
                , onInput toMsg
                ]
                []
            , iconDollarSign
            ]
        ]


iconDollarSign : Html msg
iconDollarSign =
    span [ class Bu.icon, class Bu.isSmall, class Bu.isLeft ]
        [ Fa.icon Fa.dollarSign ]


viewClearButton : Html Msg
viewClearButton =
    let
        text_ =
            "Clear"
    in
    div [ class Bu.field, class Bu.isGrouped, class Bu.isGroupedRight ]
        [ div [ class Bu.control ]
            [ button [ class Bu.isLink, class Bu.button ] [ text text_ ] ]
        ]


viewFormHelp : Model -> Html msg
viewFormHelp model =
    let
        text_ =
            "Press [Enter/Return] to clear"
    in
    p [ class Bu.isLink, class Bu.help ] [ text text_ ]


viewCalculation : Model -> Html msg
viewCalculation model =
    let
        fees =
            AttorneyFees.fromJudgmentAmount (amount model) (interest model)

        size =
            Bu.isSize4

        rightP text_ =
            p [ class size, class Bu.hasTextRight ] [ text text_ ]

        leftP text_ =
            p [ class size, class Bu.hasTextLeft ] [ text text_ ]
    in
    div [ class Bu.column, class Bu.card ]
        [ div [ class Bu.columns, class Bu.cardContent ]
            [ div
                [ class Bu.column
                , class Bu.is5Tablet
                , class Bu.is4Desktop
                , class Bu.is3Widescreen
                ]
                [ rightP <| "$ " ++ formatMoney (amount model)
                , rightP <| "+ $ " ++ formatMoney (interest model)
                , rightP <| "= $ " ++ formatMoney fees
                ]
            , div
                [ class Bu.column
                , class Bu.is4Tablet
                , class Bu.is3Desktop
                , class Bu.is2Widescreen
                ]
                [ leftP "judgment"
                , leftP "interest"
                , leftP "fees"
                ]
            ]
        ]


viewSuggestions : Model -> Html msg
viewSuggestions model =
    div [ class Bu.column ]
        [ span [] [ text "Email " ]
        , a [ href <| "mailto:" ++ model.contact.email ] [ text model.contact.name ]
        , span [] [ text " with bugs or suggestions." ]
        ]


formatMoney : String -> String
formatMoney string =
    let
        locale =
            { usLocale | decimals = 2 }

        isFloaty c =
            Char.isDigit c || c == '.'
    in
    string
        |> String.filter isFloaty
        |> String.toFloat
        |> Maybe.withDefault 0
        |> FormatNumber.format locale
