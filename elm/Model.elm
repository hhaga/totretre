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
    { kupong : List KampTips }


type Msg
    = ClearKupong
    | GenerateKupong
    | HUBMarking KampTips
    | SikkerhetMarking KampTips


type alias KupongKamp =
    ( Int, Gardering )


type alias Kampkryss =
    List ( Int, Markering )
