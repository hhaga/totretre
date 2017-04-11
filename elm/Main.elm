module Main exposing (..)

import Model exposing (..)
import Html exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClearKupong ->
            ( init, Cmd.none )

        GenerateKupong ->
            ( { kupong = [] }, Cmd.none )


view : Model -> Html Msg
view model =
    text "html"


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
