module Main exposing (main)

import Browser
import Html exposing (Html, br, button, div, input, option, select, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Random


type Msg
    = UpdateInput String
    | Init Int
    | Reset
    | UpdateComparison String


type alias Model =
    { guess : Maybe Int, comparison : Order, targetAmount : Int }


main : Program () Model Msg
main =
    Browser.element
        { init = always initialModel
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


maxReserve =
    1000


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { guess = Nothing
      , comparison = LT
      , targetAmount = 0
      }
    , Random.generate Init <| Random.int 0 maxReserve
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput guessString ->
            ( { model
                | guess = String.toInt guessString
              }
            , Cmd.none
            )

        Init amount ->
            ( { model | targetAmount = amount }, Cmd.none )

        Reset ->
            ( model, Random.generate Init <| Random.int 0 maxReserve )

        UpdateComparison string ->
            case stringToOrder string of
                Just order ->
                    ( { model | comparison = order }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text "The amount in the reserves is "
        , select [ onInput UpdateComparison ] <| List.map comparisonOption [ LT, EQ, GT ]
        , input [ onInput UpdateInput ] [ model.guess |> Maybe.map String.fromInt |> Maybe.withDefault "" |> text ]
        , text " billion dollars"
        , br [] []
        , div [] [ text <| hint model.targetAmount model.comparison model.guess ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]


hint target comparison guess =
    let
        op =
            case comparison of
                LT ->
                    (<)

                EQ ->
                    (==)

                GT ->
                    (>)
    in
    case guess of
        Nothing ->
            "No POFMA"

        Just v ->
            if op target v then
                "No POFMA"

            else
                "POFMA"


comparisonOption : Order -> Html msg
comparisonOption order =
    let
        asString =
            orderToString order
    in
    option [ value asString ] [ text asString ]


orderToString : Order -> String
orderToString order =
    case order of
        LT ->
            "<"

        EQ ->
            "="

        GT ->
            ">"


stringToOrder : String -> Maybe Order
stringToOrder string =
    case string of
        "<" ->
            Just LT

        "=" ->
            Just EQ

        ">" ->
            Just GT

        _ ->
            Nothing
