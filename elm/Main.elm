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


usikreKamper : List KampTips -> List KampTips
usikreKamper tips =
    List.filter (\k -> k.sik == False) tips


usikreKampNr : List KampTips -> List String
usikreKampNr tips =
    List.map (\k -> k.nr) (usikreKamper tips)


combineNrAndGardering : List KampTips -> List KupongSetup -> List KupongSetup2
combineNrAndGardering tips kupongUtgSetups =
    List.map (\ks -> { number = ks.number, setup = (List.map2 (\x y -> { nr = x, gardering = y }) (usikreKampNr tips) ks.setup) }) kupongUtgSetups


createKupongs : List KampTips -> List ResultatKupong
createKupongs tips =
    let
        generateHelper tips kupongSetups =
            List.map (\ks -> produserKupong tips ks) kupongSetups
    in
        generateHelper tips (combineNrAndGardering tips toTreTreSetup)


produserKupong : List KampTips -> KupongSetup2 -> ResultatKupong
produserKupong tips kupongSetup =
    let
        kupongMarkeringer tips kupongSetup kupong =
            case tips of
                [] ->
                    kupong

                head :: tail ->
                    case head.sik of
                        True ->
                            kupongMarkeringer tail kupongSetup (kupong ++ [ { nr = head.nr, markeringer = markeringForKamp head.x EnkelUtg } ])

                        False ->
                            case kupongSetup of
                                [] ->
                                    kupong

                                kupongkamp :: lasttail ->
                                    kupongMarkeringer tail lasttail (kupong ++ [ { nr = kupongkamp.nr, markeringer = markeringForKamp head.x kupongkamp.gardering } ])
    in
        { kupongNr = kupongSetup.number, kampkryss = List.sortBy .nr (kupongMarkeringer tips kupongSetup.setup []) }


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
                    , input [ type_ "radio", name gameNumber, onClick (HUBMarking { nr = gameNumber, sik = False, x = H }), id (gameNumber ++ "H"), checked (tipMarked gameNumber H kupong) ] []
                    , label [ class "label H", for (gameNumber ++ "H") ] []
                    , input [ type_ "radio", name gameNumber, onClick (HUBMarking { nr = gameNumber, sik = False, x = U }), id (gameNumber ++ "U"), checked (tipMarked gameNumber U kupong) ] []
                    , label [ class "label U", for (gameNumber ++ "U") ] []
                    , input [ type_ "radio", name gameNumber, onClick (HUBMarking { nr = gameNumber, sik = False, x = B }), id (gameNumber ++ "B"), checked (tipMarked gameNumber B kupong) ] []
                    , label [ class "label B", for (gameNumber ++ "B") ] []
                    , label [ class " switch", for (gameNumber ++ "Sikker") ]
                        [ input [ type_ "checkbox", name gameNumber, onClick (SikkerhetMarking { nr = gameNumber, sik = True, x = H }), id (gameNumber ++ "Sikker") ] []
                        , div [ class "slider round" ] []
                        ]
                    , span
                        [ class
                            (if (List.member gameNumber (usikreKampNr kupong)) then
                                ""
                             else
                                "hidden"
                            )
                        ]
                        [ text "Utgangspunkt " ]
                    , span
                        [ class
                            (if (List.member gameNumber (usikreKampNr kupong)) then
                                "hidden"
                             else
                                ""
                            )
                        ]
                        [ text "* Sikker" ]
                    ]
            )


marked : String -> Markering -> List Kampkryss -> Bool
marked gameNumber mark kupong =
    let
        kamp =
            List.filter (\k -> k.nr == gameNumber) kupong
    in
        List.member mark (List.concat (List.map (\k -> k.markeringer) kamp))


resultatKupongerRowsView : List String -> List ResultatKupong -> List (Html Msg)
resultatKupongerRowsView gameNumbers kuponger =
    List.map
        (\kupong ->
            (div [ class "kupong", id ("kupong" ++ kupong.kupongNr) ]
                (div [ class "kupongNummer" ] [ text kupong.kupongNr ]
                    :: List.map
                        (\gameNumber ->
                            div [ class "row" ]
                                [ div [ class "number" ] [ text gameNumber ]
                                , input [ type_ "checkbox", name gameNumber, checked (marked gameNumber H kupong.kampkryss), disabled True ] []
                                , input [ type_ "checkbox", name gameNumber, checked (marked gameNumber U kupong.kampkryss), disabled True ] []
                                , input [ type_ "checkbox", name gameNumber, checked (marked gameNumber B kupong.kampkryss), disabled True ] []
                                ]
                        )
                        gameNumbers
                )
            )
        )
        kuponger


view : Model -> Html Msg
view model =
    div []
        [ div [ class "tip_site" ]
            [ div [ class "tip" ] (tipsRowsView gameNumbers model.kupong)
            , button [ class "button", onClick CreateAndShow ] [ text "Generer kuponger" ]
            ]
        , iframe [ src "https://norsk-tipping.no/sport/tipping", class "iframe" ]
            []
        , div [ id "content" ] (resultatKupongerRowsView gameNumbers model.resultatKuponger)
        ]


init : Model
init =
    { kupong = List.map (\g -> { nr = g, sik = False, x = H }) gameNumbers
    , resultatKuponger = []
    }


main =
    Html.program
        { init = ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \s -> Sub.none
        }
