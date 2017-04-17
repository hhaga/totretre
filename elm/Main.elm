module Main exposing (..)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClearKupong ->
            ( init, Cmd.none )

        GenerateKupong ->
            ( { kupong = [] }, Cmd.none )

        Marking marking ->
            ( { model | kupong = marking :: model.kupong }, Cmd.none )


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
                            , input [ type_ "radio", name gameNumber, onClick (Marking ( gameNumber, Sikker H )) ] []
                            , input [ type_ "radio", name gameNumber, onClick (Marking ( gameNumber, Sikker U )) ] []
                            , input [ type_ "radio", name gameNumber, onClick (Marking ( gameNumber, Sikker B )) ] []
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
