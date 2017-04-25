module Main exposing (..)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


toTreTreSetup =
    [ [ EnkelUtg, EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg ] --1*)
    , [ EnkelUtg, HalvUtenUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg ] --2*)
    , [ EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg ] --3*)
    , [ EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg, EnkelUtg ] --4*)
    , [ HalvUtenUtg, EnkelUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg ] --5*)
    , [ HalvUtenUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg ] --6*)
    , [ HalvUtenUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg, EnkelUtg ] --7*)
    , [ HalvUtenUtg, HalvUtenUtg, EnkelUtg, EnkelUtg, EnkelUtg, HalvUtenUtg ] --8*)
    , [ HalvUtenUtg, HalvUtenUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, EnkelUtg ] --9*)
    , [ HalvUtenUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg, EnkelUtg, EnkelUtg ] --10*)
    , [ EnkelUtg, EnkelUtg, EnkelUtg, Heil, HalvUtenUtg, HalvUtenUtg ] --11*)
    , [ EnkelUtg, EnkelUtg, Heil, HalvUtenUtg, EnkelUtg, HalvUtenUtg ] --12*)
    , [ EnkelUtg, EnkelUtg, Heil, HalvUtenUtg, HalvUtenUtg, EnkelUtg ] --13*)
    , [ HalvUtenUtg, EnkelUtg, Heil, EnkelUtg, EnkelUtg, HalvUtenUtg ] --14*)
    , [ HalvUtenUtg, EnkelUtg, Heil, EnkelUtg, HalvUtenUtg, EnkelUtg ] --15*)
    , [ HalvUtenUtg, EnkelUtg, Heil, HalvUtenUtg, EnkelUtg, EnkelUtg ] --16*)
    , [ EnkelUtg, Heil, Heil, EnkelUtg, EnkelUtg, HalvUtenUtg ] --17*)
    , [ EnkelUtg, Heil, Heil, EnkelUtg, HalvUtenUtg, EnkelUtg ] --18*)
    , [ EnkelUtg, Heil, Heil, HalvUtenUtg, EnkelUtg, EnkelUtg ] --19*)
    , [ Heil, Heil, Heil, EnkelUtg, EnkelUtg, EnkelUtg ] --20
    ]


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

        tail_rec_kupong_setups kupongUtgSetups kupongNr acc =
            case kupongUtgSetups of
                [] ->
                    acc

                head :: tail ->
                    tail_rec_kupong_setups tail (toString ((Result.withDefault 0 (String.toInt kupongNr)) + 1)) (List.map2 ((,)) usikreKampNr head :: acc)

        tail_helper tips kupongSetups kuponger =
            case kupongSetups of
                [] ->
                    kuponger

                head :: tail ->
                    tail_helper tips tail ((produserKupong tips head []) :: kuponger)
    in
        tail_helper tips (tail_rec_kupong_setups toTreTreSetup "1" []) []


produserKupong : List KampTips -> List KupongKamp -> List Kampkryss -> List Kampkryss
produserKupong tips kupongSetup kupong =
    case tips of
        [] ->
            kupong

        head :: tail ->
            case head.sik of
                Sikker ->
                    produserKupong tail kupongSetup kupong ++ [ ( head.nr, (markeringForKamp head.x EnkelUtg) ) ]

                Utgangspunkt ->
                    case kupongSetup of
                        [] ->
                            kupong

                        ( j, markValg ) :: lasttail ->
                            produserKupong tail kupongSetup kupong ++ [ ( j, (markeringForKamp head.x markValg) ) ]


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


kupongRowsView gameNumbers =
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


kupongHeaderView =
    [ div [ class "number" ] [ text " " ]
    , div [ class "leftcell marking" ] [ text "H" ]
    , div [ class "middlecell marking" ] [ text "U" ]
    , div [ class "rightcell marking" ] [ text "B" ]
    ]


gameNumbers =
    [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12" ]


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
            ]


init : Model
init =
    { kupong = [], resultatKuponger = [] }


main =
    Html.program
        { init = ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \s -> Sub.none
        }
