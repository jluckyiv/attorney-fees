module Main exposing (main)

import AttorneyFees
import Browser
import Browser.Dom as Dom
import Bulma.Classes as Bu
import Date exposing (Date)
import FontAwesome as Fa
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Helpers
import Html exposing (Html, a, button, div, form, h1, h2, input, label, p, section, span, text)
import Html.Attributes exposing (class, href, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Interest
import Rate exposing (Rate)
import Task
import Time



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
    { name : String, email : String, today : Int }


type alias Config =
    { name : String, email : String }


type alias Model =
    { config : Config
    , data : Data
    , today : Date
    }


type Data
    = Current Amount Interest Start End
    | Cached Amount Interest Start End


type alias Amount =
    String


type alias Interest =
    String


type alias Start =
    String


type alias End =
    String


type alias ViewInputParams =
    { toMsg : String -> Msg
    , inputId : String
    , inputValue : String
    , inputPlaceholder : String
    , inputLabel : String
    }


defaultInterest : String
defaultInterest =
    "10.0"


init : Flags -> ( Model, Cmd Msg )
init { name, email, today } =
    ( { config = { name = name, email = email }
      , data = Current "" defaultInterest "" ""
      , today = Time.millisToPosix today |> Date.fromPosix Time.utc
      }
    , Cmd.batch [ focus judgmentInputId, Date.today |> Task.perform ReceivedToday ]
    )


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
    | UpdatedStartInput String
    | UpdatedEndInput String
    | ReceivedToday Date


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
            ( updateInterestRate model string, Cmd.none )

        UpdatedStartInput string ->
            ( updateStartDate model string, Cmd.none )

        UpdatedEndInput string ->
            ( updateEndDate model string, Cmd.none )

        ReceivedToday date_ ->
            ( { model | today = date_ }
            , Cmd.none
            )


updateJudgmentAmount : Model -> String -> Model
updateJudgmentAmount model string =
    case model.data of
        Current _ _ _ _ ->
            { model | data = Current string (interest model) (start model) (end model) }

        Cached _ _ _ _ ->
            { model | data = Current string "" "" "" }


updateInterestRate : Model -> String -> Model
updateInterestRate model string =
    { model | data = Current (amount model) string (start model) (end model) }


updateStartDate : Model -> String -> Model
updateStartDate model string =
    { model | data = Current (amount model) (interest model) string (end model) }


updateEndDate : Model -> String -> Model
updateEndDate model string =
    { model | data = Current (amount model) (interest model) (start model) string }



---- HELPERS ----


amount : Model -> Amount
amount model =
    case model.data of
        Current a _ _ _ ->
            a

        Cached a _ _ _ ->
            a


interest : Model -> Interest
interest model =
    case model.data of
        Current _ i _ _ ->
            i

        Cached _ i _ _ ->
            i


interestCalculation : Model -> Interest
interestCalculation model =
    case model.data of
        Current j i f t ->
            Interest.calculateFromStrings j i f t
                |> Interest.format

        Cached j i f t ->
            Interest.calculateFromStrings j i f t
                |> Interest.format


start : Model -> Interest
start model =
    case model.data of
        Current _ _ s _ ->
            s

        Cached _ _ s _ ->
            s


end : Model -> Interest
end model =
    case model.data of
        Current _ _ _ e ->
            e

        Cached _ _ _ e ->
            e


clear : Model -> Model
clear model =
    case model.data of
        Current a i s e ->
            { model | data = Cached a i s e }

        Cached _ _ _ _ ->
            { model | data = Current "" "" "" "" }


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
                Current a _ _ _ ->
                    a

                Cached _ _ _ _ ->
                    ""

        inputLabel =
            "Judgment"

        inputPlaceholder =
            "Judgment amount"

        toMsg =
            UpdatedJudgmentInput
    in
    viewDollarInput (ViewInputParams toMsg judgmentInputId inputValue inputPlaceholder inputLabel)


viewInterestInput : Model -> Html Msg
viewInterestInput model =
    let
        inputValue =
            case model.data of
                Current _ i _ _ ->
                    i

                Cached _ _ _ _ ->
                    ""
    in
    div []
        [ viewDateInput (ViewInputParams UpdatedStartInput "start" (start model) ("e.g., " ++ Date.toIsoString model.today) "Interest start")
        , viewDateInput (ViewInputParams UpdatedEndInput "end" (end model) ("e.g., " ++ Date.toIsoString model.today) "Interest end")
        , viewPercentInput (ViewInputParams UpdatedInterestInput "rate" (interest model) "e.g., 10.0" "Interest rate")
        ]


viewDollarInput : ViewInputParams -> Html Msg
viewDollarInput { toMsg, inputId, inputValue, inputPlaceholder, inputLabel } =
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
                , class Bu.hasTextRight
                , onInput toMsg
                ]
                []
            , iconDollarSign
            ]
        ]


viewPercentInput : ViewInputParams -> Html Msg
viewPercentInput { toMsg, inputId, inputValue, inputPlaceholder, inputLabel } =
    let
        inputType =
            "text"
    in
    div [ class Bu.field ]
        [ label [ class Bu.label ] [ text inputLabel ]
        , div [ class Bu.control, class Bu.hasIconsRight ]
            [ input
                [ id inputId
                , type_ inputType
                , placeholder inputPlaceholder
                , value inputValue
                , class Bu.input
                , class Bu.hasTextRight
                , onInput toMsg
                ]
                []
            , iconPercent
            ]
        ]


viewDateInput : ViewInputParams -> Html Msg
viewDateInput { toMsg, inputId, inputValue, inputPlaceholder, inputLabel } =
    let
        inputType =
            "text"
    in
    div [ class Bu.field ]
        [ label [ class Bu.label ] [ text inputLabel ]
        , div [ class Bu.control, class Bu.hasIconsRight ]
            [ input
                [ id inputId
                , type_ inputType
                , placeholder inputPlaceholder
                , value inputValue
                , class Bu.input
                , class Bu.hasTextRight
                , onInput toMsg
                ]
                []
            ]
        ]


iconPercent : Html msg
iconPercent =
    span [ class Bu.icon, class Bu.isSmall, class Bu.isRight ]
        [ Fa.icon Fa.percent ]


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
            AttorneyFees.fromJudgmentAmount (amount model) (interestCalculation model)

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
                [ rightP <| "$ " ++ Helpers.formatString (amount model)
                , rightP <| "+ $ " ++ Helpers.formatString (interestCalculation model)
                , rightP <| "= $ " ++ Helpers.formatString fees
                ]
            , div
                [ class Bu.column
                , class Bu.is5Tablet
                , class Bu.is4Desktop
                , class Bu.is3Widescreen
                ]
                [ leftP "judgment"
                , leftP "max interest"
                , leftP "max fees"
                ]
            ]
        ]


viewSuggestions : Model -> Html msg
viewSuggestions model =
    div [ class Bu.column ]
        [ span [] [ text "Email " ]
        , a [ href <| "mailto:" ++ model.config.email ] [ text model.config.name ]
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
