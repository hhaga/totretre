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
    { kupong : List KampTips, resultatKuponger : List ResultatKupong }


type Msg
    = ClearKupong
    | HUBMarking KampTips
    | SikkerhetMarking KampTips
    | CreateAndShow


type alias KupongKamp =
    { nr : String, gardering : Gardering }


type alias Kampkryss =
    { nr : String, markeringer : List Markering }


type alias ResultatKupong =
    { kupongNr : String, kampkryss : List Kampkryss }


type alias KupongSetup =
    { number : String, setup : List Gardering }


type alias KupongSetup2 =
    { number : String, setup : List KupongKamp }


toTreTreSetup : List KupongSetup
toTreTreSetup =
    [ { number = "1", setup = [ EnkelUtg, EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg ] } --1*)
    , { number = "2", setup = [ EnkelUtg, HalvUtenUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg ] } --2*)
    , { number = "3", setup = [ EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg ] } --3*)
    , { number = "4", setup = [ EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg, EnkelUtg ] } --4*)
    , { number = "5", setup = [ HalvUtenUtg, EnkelUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg ] } --5*)
    , { number = "6", setup = [ HalvUtenUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg ] } --6*)
    , { number = "7", setup = [ HalvUtenUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg, EnkelUtg ] } --7*)
    , { number = "8", setup = [ HalvUtenUtg, HalvUtenUtg, EnkelUtg, EnkelUtg, EnkelUtg, HalvUtenUtg ] } --8*)
    , { number = "9", setup = [ HalvUtenUtg, HalvUtenUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, EnkelUtg ] } --9*)
    , { number = "10", setup = [ HalvUtenUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg, EnkelUtg, EnkelUtg ] } --10*)
    , { number = "11", setup = [ EnkelUtg, EnkelUtg, EnkelUtg, Heil, HalvUtenUtg, HalvUtenUtg ] } --11*)
    , { number = "12", setup = [ EnkelUtg, EnkelUtg, Heil, HalvUtenUtg, EnkelUtg, HalvUtenUtg ] } --12*)
    , { number = "13", setup = [ EnkelUtg, EnkelUtg, Heil, HalvUtenUtg, HalvUtenUtg, EnkelUtg ] } --13*)
    , { number = "14", setup = [ HalvUtenUtg, EnkelUtg, Heil, EnkelUtg, EnkelUtg, HalvUtenUtg ] } --14*)
    , { number = "15", setup = [ HalvUtenUtg, EnkelUtg, Heil, EnkelUtg, HalvUtenUtg, EnkelUtg ] } --15*)
    , { number = "16", setup = [ HalvUtenUtg, EnkelUtg, Heil, HalvUtenUtg, EnkelUtg, EnkelUtg ] } --16*)
    , { number = "17", setup = [ EnkelUtg, Heil, Heil, EnkelUtg, EnkelUtg, HalvUtenUtg ] } --17*)
    , { number = "18", setup = [ EnkelUtg, Heil, Heil, EnkelUtg, HalvUtenUtg, EnkelUtg ] } --18*)
    , { number = "19", setup = [ EnkelUtg, Heil, Heil, HalvUtenUtg, EnkelUtg, EnkelUtg ] } --19*)
    , { number = "20", setup = [ Heil, Heil, Heil, EnkelUtg, EnkelUtg, EnkelUtg ] } --20
    ]


gameNumbers =
    [ ".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "10", "11", "12" ]
