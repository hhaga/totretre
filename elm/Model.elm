module Model exposing (..)


type Markering
    = H
    | U
    | B


type Sikkerhet
    = Sikker Markering
    | Utgangspunkt Markering


type Gardering
    = EnkelUtg
    | HalvUtenUtg
    | Heil


type alias KampTips =
    Int Sikkerhet


type alias Model =
    { kupong : List KampTips }


type Msg
    = ClearKupong
    | GenerateKupong


type alias KupongKamp =
    Int Gardering


type alias Kampkryss =
    List (Int Markering)
