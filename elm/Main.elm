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


getCurrentKamp : String -> List KampTips -> List KampTips
getCurrentKamp nr kupong =
    List.filter (\k -> k.nr == nr) kupong


updateCurrentKamp : KampTips -> List KampTips -> List KampTips -> List KampTips
updateCurrentKamp oldKampTips updatedKamptips kupong =
    updatedKamptips ++ (List.filter (\k -> k.nr /= oldKampTips.nr) kupong)


updateHUB : KampTips -> List KampTips -> List KampTips
updateHUB kt kupong =
    let
        kamptips =
            getCurrentKamp kt.nr kupong

        updatedKamptips =
            List.map (\k -> { k | x = kt.x }) kamptips
    in
        updateCurrentKamp kt updatedKamptips kupong


updateSikkerhet : KampTips -> List KampTips -> List KampTips
updateSikkerhet kt kupong =
    let
        kamptips =
            getCurrentKamp kt.nr kupong

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
            List.map (\ks -> List.sortBy .nr (produserKupong tips ks [])) kupongSetups
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
                    produserKupong tail kupongSetup (kupong ++ [ { nr = head.nr, markeringer = markeringForKamp head.x EnkelUtg } ])

                False ->
                    case kupongSetup of
                        [] ->
                            kupong

                        kupongkamp :: lasttail ->
                            produserKupong tail lasttail (kupong ++ [ { nr = kupongkamp.nr, markeringer = markeringForKamp head.x kupongkamp.gardering } ])


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


tipMarked : String -> Markering -> List KampTips -> Bool
tipMarked gameNumber mark kupong =
    let
        kamp =
            getCurrentKamp gameNumber kupong
    in
        not (List.isEmpty (List.filter (\k -> k.x == mark) kamp))


tipsRowsView : List String -> List KampTips -> List (Html Msg)
tipsRowsView gameNumbers kupong =
    gameNumbers
        |> List.map
            (\gameNumber ->
                div [ class "row" ]
                    [ div [ class "number" ] [ text gameNumber ]
                    , input [ type_ "radio", name gameNumber, onClick (HUBMarking { nr = gameNumber, sik = False, x = H }), checked (tipMarked gameNumber H kupong) ] []
                    , input [ type_ "radio", name gameNumber, onClick (HUBMarking { nr = gameNumber, sik = False, x = U }), checked (tipMarked gameNumber U kupong) ] []
                    , input [ type_ "radio", name gameNumber, onClick (HUBMarking { nr = gameNumber, sik = False, x = B }), checked (tipMarked gameNumber B kupong) ] []
                    , input [ type_ "checkbox", name gameNumber, onClick (SikkerhetMarking { nr = gameNumber, sik = True, x = H }) ] []
                    ]
            )


marked : String -> Markering -> List Kampkryss -> Bool
marked gameNumber mark kupong =
    let
        kamp =
            List.filter (\k -> k.nr == gameNumber) kupong
    in
        List.member mark (List.concat (List.map (\k -> k.markeringer) kamp))


resultatKupongerRowsView : List String -> List (List Kampkryss) -> List (Html Msg)
resultatKupongerRowsView gameNumbers kuponger =
    List.concat
        (List.map
            (\kupong ->
                ([ div [] [ text "test" ] ]
                    ++ List.map
                        (\gameNumber ->
                            div [ class "row" ]
                                [ div [ class "number" ] [ text gameNumber ]
                                , input [ type_ "checkbox", name gameNumber, checked (marked gameNumber H kupong) ] []
                                , input [ type_ "checkbox", name gameNumber, checked (marked gameNumber U kupong) ] []
                                , input [ type_ "checkbox", name gameNumber, checked (marked gameNumber B kupong) ] []
                                ]
                        )
                        gameNumbers
                )
            )
            kuponger
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
            tipsRowsView gameNumbers model.kupong
    in
        div [ class "rows" ]
            [ div [] kupongHeaderView
            , div [] rows
            , button [ onClick CreateAndShow ] [ text "Generer kuponger" ]
            , div [] (resultatKupongerRowsView gameNumbers model.resultatKuponger)
            ]


init : Model
init =
    { kupong =
        [ { nr = ".1", sik = False, x = H }
        , { nr = ".2", sik = False, x = H }
        , { nr = ".3", sik = False, x = H }
        , { nr = ".4", sik = False, x = H }
        , { nr = ".5", sik = False, x = H }
        , { nr = ".6", sik = False, x = H }
        , { nr = ".7", sik = False, x = H }
        , { nr = ".8", sik = False, x = H }
        , { nr = ".9", sik = False, x = H }
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
