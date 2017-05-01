module Model exposing (..)


type Markering
    = H
    | U
    | B


type Sikkerhet
    = Sikker
    | Utgangspunkt


type Gardering
    = EnkelUtg
    | HalvUtenUtg
    | Heil


type alias KampTips =
    { nr : String, sik : Bool, x : Markering }


type alias Model =
    { kupong : List KampTips, resultatKuponger : List (List Kampkryss) }


type Msg
    = ClearKupong
    | HUBMarking KampTips
    | SikkerhetMarking KampTips
    | CreateAndShow


type alias KupongKamp =
    { nr : String, gardering : Gardering }


type alias Kampkryss =
    { nr : String, markeringer : List Markering }


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


gameNumbers =
    [ ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "10", "11", "12" ]
