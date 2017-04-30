module Main exposing (..)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


markeringForKamp : Markering -> Gardering -> List Markering
markeringForKamp utgangspunkt systemvalg =
    case systemvalg of
        EnkelUtg ->
            [ utgangspunkt ]

        HalvUtenUtg ->
            List.filter (\x -> x /= utgangspunkt) [ H, U, B ]

        Heil ->
            [ H, U, B ]


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


createKupongs : List KampTips -> List (List Kampkryss)
createKupongs tips =
    let
        usikre =
            List.filter (\k -> k.sik == Utgangspunkt) tips

        usikreKampNr =
            List.map (\k -> k.nr) usikre

        combineNrAndGardering kupongUtgSetups =
            List.map (\ks -> (List.map2 (\x y -> { nr = x, gardering = y }) usikreKampNr ks)) kupongUtgSetups

        generateHelper tips kupongSetups =
            List.map (\ks -> produserKupong tips ks) kupongSetups
    in
        generateHelper tips (combineNrAndGardering toTreTreSetup)


produserKupong : List KampTips -> List KupongKamp -> List Kampkryss
produserKupong tips kupongSetup =
    List.concat
        (List.map
            (\t ->
                case t.sik of
                    Sikker ->
                        [ ( t.nr, markeringForKamp t.x EnkelUtg ) ]

                    Utgangspunkt ->
                        List.map (\ks -> ( ks.nr, markeringForKamp t.x ks.gardering )) kupongSetup
            )
            tips
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClearKupong ->
            ( init, Cmd.none )

        HUBMarking kamptips ->
            ( { model | kupong = updateHUB kamptips model.kupong }, Cmd.none )

        SikkerhetMarking kamptips ->
            ( { model | kupong = updateSikkerhet kamptips model.kupong }, Cmd.none )

        CreateAndShow ->
            ( { model | resultatKuponger = createKupongs model.kupong }, Cmd.none )


testKupongSetup =
    [ [ EnkelUtg, EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg ]
    , [ EnkelUtg, HalvUtenUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg ]
    ]


kupongRowsView gameNumbers =
    gameNumbers
        |> List.map
            (\gameNumber ->
                div [ class "row" ]
                    [ div [ class "number" ] [ text gameNumber ]
                    , input [ type_ "radio", name gameNumber, onClick (HUBMarking { nr = gameNumber, sik = Utgangspunkt, x = H }) ] []
                    , input [ type_ "radio", name gameNumber, onClick (HUBMarking { nr = gameNumber, sik = Utgangspunkt, x = U }) ] []
                    , input [ type_ "radio", name gameNumber, onClick (HUBMarking { nr = gameNumber, sik = Utgangspunkt, x = B }) ] []
                    , input [ type_ "checkbox", name gameNumber, onClick (SikkerhetMarking { nr = gameNumber, sik = Sikker, x = H }) ] []
                    ]
            )


kupongHeaderView =
    [ div [ class "number" ] [ text " " ]
    , div [ class "leftcell marking" ] [ text "H" ]
    , div [ class "middlecell marking" ] [ text "U" ]
    , div [ class "rightcell marking" ] [ text "B" ]
    ]


view : Model -> Html Msg
view model =
    let
        rows =
            kupongRowsView gameNumbers
    in
        div [ class "rows" ]
            [ div [] kupongHeaderView
            , div [] rows
            , button [ onClick CreateAndShow ] [ text "+" ]
            , div [] [ text (toString model.resultatKuponger) ]
            , div [] [ text (toString model.kupong) ]
            , div []
                [ text
                    (toString
                        ( produserKupong [ { nr = "1", sik = Utgangspunkt, x = H }, { nr = "2", sik = Sikker, x = H } ]
                        , testKupongSetup
                        , []
                        )
                    )
                ]
            ]


init : Model
init =
    { kupong =
        [ { nr = "1", sik = Utgangspunkt, x = H }
        , { nr = "2", sik = Utgangspunkt, x = H }
        , { nr = "3", sik = Utgangspunkt, x = H }
        , { nr = "4", sik = Utgangspunkt, x = H }
        , { nr = "5", sik = Utgangspunkt, x = H }
        , { nr = "6", sik = Utgangspunkt, x = H }
        , { nr = "7", sik = Utgangspunkt, x = H }
        , { nr = "8", sik = Utgangspunkt, x = H }
        , { nr = "9", sik = Utgangspunkt, x = H }
        , { nr = "10", sik = Utgangspunkt, x = H }
        , { nr = "11", sik = Utgangspunkt, x = H }
        , { nr = "12", sik = Utgangspunkt, x = H }
        ]
    , resultatKuponger = []
    }


main =
    Html.program
        { init = ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \s -> Sub.none
        }
