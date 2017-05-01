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
            List.map (\k -> { k | sik = not k.sik }) kamptips
    in
        updateCurrentKamp kt updatedKamptips kupong


usikre : List KampTips -> List KampTips
usikre tips =
    List.filter (\k -> k.sik == False) tips


usikreKampNr : List KampTips -> List String
usikreKampNr tips =
    List.map (\k -> k.nr) (usikre tips)


combineNrAndGardering : List KampTips -> List (List Gardering) -> List (List KupongKamp)
combineNrAndGardering tips kupongUtgSetups =
    List.map (\ks -> (List.map2 (\x y -> { nr = x, gardering = y }) (usikreKampNr tips) ks)) kupongUtgSetups


createKupongs : List KampTips -> List (List Kampkryss)
createKupongs tips =
    let
        generateHelper tips kupongSetups =
            List.map (\ks -> produserKupong tips ks []) kupongSetups
    in
        generateHelper tips (combineNrAndGardering tips toTreTreSetup)


produserKupong : List KampTips -> List KupongKamp -> List Kampkryss -> List Kampkryss
produserKupong tips kupongSetup kupong =
    case tips of
        [] ->
            kupong

        head :: tail ->
            case head.sik of
                True ->
                    produserKupong tail kupongSetup (kupong ++ [ ( head.nr, markeringForKamp head.x EnkelUtg ) ])

                False ->
                    case kupongSetup of
                        [] ->
                            kupong

                        kupongkamp :: lasttail ->
                            produserKupong tail lasttail (kupong ++ [ ( kupongkamp.nr, markeringForKamp head.x kupongkamp.gardering ) ])


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


kupongRowsView : List String -> List (Html Msg)
kupongRowsView gameNumbers =
    gameNumbers
        |> List.map
            (\gameNumber ->
                div [ class "row" ]
                    [ div [ class "number" ] [ text gameNumber ]
                    , input [ type_ "radio", name gameNumber, onClick (HUBMarking { nr = gameNumber, sik = False, x = H }) ] []
                    , input [ type_ "radio", name gameNumber, onClick (HUBMarking { nr = gameNumber, sik = False, x = U }) ] []
                    , input [ type_ "radio", name gameNumber, onClick (HUBMarking { nr = gameNumber, sik = False, x = B }) ] []
                    , input [ type_ "checkbox", name gameNumber, onClick (SikkerhetMarking { nr = gameNumber, sik = True, x = H }) ] []
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
            ]


init : Model
init =
    { kupong =
        [ { nr = "1", sik = False, x = H }
        , { nr = "2", sik = False, x = H }
        , { nr = "3", sik = False, x = H }
        , { nr = "4", sik = False, x = H }
        , { nr = "5", sik = False, x = H }
        , { nr = "6", sik = False, x = H }
        , { nr = "7", sik = False, x = H }
        , { nr = "8", sik = False, x = H }
        , { nr = "9", sik = False, x = H }
        , { nr = "10", sik = False, x = H }
        , { nr = "11", sik = False, x = H }
        , { nr = "12", sik = False, x = H }
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
