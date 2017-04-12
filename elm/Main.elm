module Main exposing (..)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClearKupong ->
            ( init, Cmd.none )

        GenerateKupong ->
            ( { kupong = [] }, Cmd.none )


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
                            , div [ class "leftcell marking" ] [ text "H" ]
                            , div [ class "middlecell marking" ] [ text "U" ]
                            , div [ class "rightcell marking" ] [ text "B" ]
                            ]
                    )
    in
        div [ class "rows" ] rows


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
