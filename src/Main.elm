module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)
import Hex

main =
    Browser.sandbox { init = init, update = update, view = view }

type alias Model =
    { bits: List Bool
    }

init : Model
init =
    Model (List.repeat 48 False)

type Msg
    = ToggleBit Int Bool

update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleBit index value ->
            { model | bits = List.indexedMap (\i x -> if i == index then value else x) model.bits }

type AddressSection
    = Offset
    | PT
    | PD
    | PDPT
    | PML4
    | Extended

addressSectionName : AddressSection -> String
addressSectionName section =
    case section of
        Offset -> "Offset"
        PT -> "PT"
        PD -> "PD"
        PDPT -> "PDPT"
        PML4 -> "PML4"
        Extended -> "Sign extension"

view : Model -> Html Msg
view model =
    div
        [ style "text-align" "center"
        ]
        [ p
            [ style "font-size" "2.5em"
            , style "font-family" "monospace"
            ]
            [ text ("0x" ++ (model.bits |> signExtend |> bitsToHex |> String.padLeft 16 '0'))
            ]
        , viewCheckboxGroup Extended model.bits
        , viewCheckboxGroup PML4 model.bits
        , viewCheckboxGroup PDPT model.bits
        , viewCheckboxGroup PD model.bits
        , viewCheckboxGroup PT model.bits
        , viewCheckboxGroup Offset model.bits
        ]

viewCheckboxGroup : AddressSection -> List Bool -> Html Msg
viewCheckboxGroup section bits =
    let
        color : String
        color =
            case section of
                Extended -> "lightgray"
                PML4 -> "khaki"
                PDPT -> "thistle"
                PD -> "lightsalmon"
                PT -> "paleturquoise"
                Offset -> "wheat"
    in
    span
        [ style "display" "inline-block"
        , style "background-color" color
        , style "text-align" "center"
        ]
        [ div [] (bits |> sectionBits section |> List.indexedMap (\i bit -> viewCheckbox section i bit) |> List.reverse)
        , div [] [ text (addressSectionName section) ]
        , div []
            [ text (case section of
                Extended -> "-"
                _ -> sectionBits section bits |> bitsToInt |> String.fromInt
            )
            ]
        ]

viewCheckbox : AddressSection -> Int -> Bool -> Html Msg
viewCheckbox section index bit =
    input
        ( type_ "checkbox" :: checked bit ::
            case section of
                Extended -> [ disabled True ]
                PML4 -> [ onCheck (ToggleBit (index + 39)) ]
                PDPT -> [ onCheck (ToggleBit (index + 30)) ]
                PD -> [ onCheck (ToggleBit (index + 21)) ]
                PT -> [ onCheck (ToggleBit (index + 12)) ]
                Offset -> [ onCheck (ToggleBit index) ]
        )
        []

sectionBits : AddressSection -> List Bool -> List Bool
sectionBits section bits =
    bits |> case section of
        Extended -> List.reverse >> List.head >> Maybe.withDefault False >> List.repeat 16
        PT -> List.drop (3 + (1 * 9)) >> List.take 9
        PD -> List.drop (3 + (2 * 9)) >> List.take 9
        PDPT -> List.drop (3 + (3 * 9)) >> List.take 9
        PML4 -> List.drop (3 + (4 * 9)) >> List.take 9
        Offset -> List.take 12

signExtend : List Bool -> List Bool
signExtend bits =
    bits ++ sectionBits Extended bits

bitsToInt : List Bool -> Int
bitsToInt bits =
    case bits of
        x :: xs -> ((bitsToInt xs) * 2) + if x then 1 else 0
        _ -> 0

bitsToHex : List Bool -> String
bitsToHex bits =
    case bits of
        a :: b :: c :: d :: xs -> bitsToHex xs ++ (bitsToInt [a, b, c, d] |> Hex.toString)
        [] -> ""
        _ -> bits ++ [ False ] |> bitsToHex
