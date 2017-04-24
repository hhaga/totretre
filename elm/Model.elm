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
    { nr : String, sik : Sikkerhet, x : Markering }


type alias Model =
    { kupong : List KampTips, resultatKuponger : List (List Kampkryss) }


type Msg
    = ClearKupong
    | HUBMarking KampTips
    | SikkerhetMarking KampTips
    | CreateAndShow


type alias KupongKamp =
    ( String, Gardering )


type alias Kampkryss =
    ( String, List Markering )
