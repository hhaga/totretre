module Main exposing (..)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


getCurrentKamp : KampTips -> List KampTips -> List KampTips
getCurrentKamp kt kupong =
    List.filter (\k -> k.nr == kt.nr) kupong


updateCurrentKamp : KampTips -> List KampTips -> List KampTips -> List KampTips
updateCurrentKamp oldKampTips updatedKamptips kupong =
    updatedKamptips ++ (List.filter (\k -> k.nr /= oldKampTips.nr) kupong)


updateHUB : KampTips -> List KampTips -> List KampTips
updateHUB kt kupong =
    let
        kamptips =
            getCurrentKamp kt kupong

        updatedKamptips =
            List.map (\k -> { k | x = kt.x }) kamptips
    in
        updateCurrentKamp kt updatedKamptips kupong


updateSikkerhet : KampTips -> List KampTips -> List KampTips
updateSikkerhet kt kupong =
    let
        kamptips =
            getCurrentKamp kt kupong

        updatedKamptips =
            List.map (\k -> { k | sik = kt.sik }) kamptips
    in
        updateCurrentKamp kt updatedKamptips kupong


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClearKupong ->
            ( init, Cmd.none )

        GenerateKupong ->
            ( { kupong = [] }, Cmd.none )

        HUBMarking kamptips ->
            ( { model | kupong = updateHUB kamptips model.kupong }, Cmd.none )

        SikkerhetMarking kamptips ->
            ( { model | kupong = updateSikkerhet kamptips model.kupong }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        gameNumbers =
            [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12" ]

        rows =
            gameNumbers
                |> List.map
                    (\gameNumber ->
                        div [ class "row" ]
                            [ div [ class "number" ] [ text gameNumber ]
                            , input [ type_ "radio", name gameNumber, onClick (HUBMarking { nr = gameNumber, sik = Utgangspunkt, x = H }) ] []
                            , input [ type_ "radio", name gameNumber, onClick (HUBMarking { nr = gameNumber, sik = Utgangspunkt, x = U }) ] []
                            , input [ type_ "radio", name gameNumber, onClick (HUBMarking { nr = gameNumber, sik = Utgangspunkt, x = B }) ] []
                            , input [ type_ "checkbox", name gameNumber, onClick (SikkerhetMarking { nr = gameNumber, sik = Utgangspunkt, x = H }) ] []
                            ]
                    )
    in
        div [ class "rows" ]
            [ div []
                [ div [ class "number" ] [ text " " ]
                , div [ class "leftcell marking" ] [ text "H" ]
                , div [ class "middlecell marking" ] [ text "U" ]
                , div [ class "rightcell marking" ] [ text "B" ]
                ]
            , div [] rows
            ]


init : Model
init =
    { kupong = [] }


main =
    Html.program
        { init = ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \s -> Sub.none
        }
