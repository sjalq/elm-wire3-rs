port module Harness exposing (main)

{-| Wire3 round-trip test harness.

Protocol (line-based over stdin/stdout via ports):
  Request:  "<typeName> <hex-encoded-wire3-bytes>"
  Response: "OK <hex-encoded-wire3-bytes>"
       or:  "ERR <error-message>"

-}

import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Bytes.Encode as BE
import Dict exposing (Dict)
import Lamdera.Wire3 as Wire3
import Platform
import Set exposing (Set)
import TestTypes exposing (..)


port requestPort : (String -> msg) -> Sub msg


port responsePort : String -> Cmd msg


type Msg
    = GotRequest String


main : Program () () Msg
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = update
        , subscriptions = \_ -> requestPort GotRequest
        }


update : Msg -> () -> ( (), Cmd Msg )
update msg _ =
    case msg of
        GotRequest line ->
            ( (), responsePort (processRequest line) )


processRequest : String -> String
processRequest line =
    case String.words line of
        typeName :: hexStr :: _ ->
            let
                bytes =
                    hexToBytes hexStr
            in
            handleType typeName bytes

        _ ->
            "ERR invalid-request-format"


handleType : String -> Bytes -> String
handleType typeName bytes =
    case typeName of
        "Person" ->
            roundTrip w3_decode_Person w3_encode_Person bytes

        "Color" ->
            roundTrip w3_decode_Color w3_encode_Color bytes

        "Shape" ->
            roundTrip w3_decode_Shape w3_encode_Shape bytes

        "Inventory" ->
            roundTrip w3_decode_Inventory w3_encode_Inventory bytes

        "Team" ->
            roundTrip w3_decode_Team w3_encode_Team bytes

        "ApiResponse" ->
            roundTrip w3_decode_ApiResponse w3_encode_ApiResponse bytes

        "Tree" ->
            roundTrip w3_decode_Tree w3_encode_Tree bytes

        "Coordinate" ->
            roundTrip w3_decode_Coordinate w3_encode_Coordinate bytes

        "Dashboard" ->
            roundTrip w3_decode_Dashboard w3_encode_Dashboard bytes

        "Int" ->
            roundTrip Wire3.decodeInt Wire3.encodeInt bytes

        "Float" ->
            roundTrip (BD.float64 Bytes.LE) (BE.float64 Bytes.LE) bytes

        "Bool" ->
            roundTrip Wire3.decodeBool Wire3.encodeBool bytes

        "String" ->
            roundTrip Wire3.decodeString Wire3.encodeString bytes

        _ ->
            "ERR unknown-type:" ++ typeName


roundTrip : BD.Decoder a -> (a -> BE.Encoder) -> Bytes -> String
roundTrip decoder encoder bytes =
    case BD.decode decoder bytes of
        Nothing ->
            "ERR decode-failed"

        Just value ->
            "OK " ++ bytesToHex (BE.encode (encoder value))



-- Hex encoding/decoding for transport over text ports


bytesToHex : Bytes -> String
bytesToHex bytes =
    let
        width =
            Bytes.width bytes
    in
    case BD.decode (hexEncodeDecoder width) bytes of
        Just hex ->
            hex

        Nothing ->
            ""


hexEncodeDecoder : Int -> BD.Decoder String
hexEncodeDecoder width =
    BD.loop ( width, [] ) hexEncodeStep


hexEncodeStep : ( Int, List String ) -> BD.Decoder (BD.Step ( Int, List String ) String)
hexEncodeStep ( remaining, acc ) =
    if remaining <= 0 then
        BD.succeed (BD.Done (String.join "" (List.reverse acc)))

    else
        BD.map
            (\byte -> BD.Loop ( remaining - 1, byteToHex byte :: acc ))
            BD.unsignedInt8


byteToHex : Int -> String
byteToHex n =
    String.fromChar (hexDigit (n // 16)) ++ String.fromChar (hexDigit (modBy 16 n))


hexDigit : Int -> Char
hexDigit n =
    case n of
        0 -> '0'
        1 -> '1'
        2 -> '2'
        3 -> '3'
        4 -> '4'
        5 -> '5'
        6 -> '6'
        7 -> '7'
        8 -> '8'
        9 -> '9'
        10 -> 'a'
        11 -> 'b'
        12 -> 'c'
        13 -> 'd'
        14 -> 'e'
        _ -> 'f'


hexToBytes : String -> Bytes
hexToBytes hex =
    BE.encode (BE.sequence (List.map BE.unsignedInt8 (hexPairs (String.toList hex))))


hexPairs : List Char -> List Int
hexPairs chars =
    case chars of
        hi :: lo :: rest ->
            (hexValue hi * 16 + hexValue lo) :: hexPairs rest

        _ ->
            []


hexValue : Char -> Int
hexValue c =
    case c of
        '0' -> 0
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        'a' -> 10
        'A' -> 10
        'b' -> 11
        'B' -> 11
        'c' -> 12
        'C' -> 12
        'd' -> 13
        'D' -> 13
        'e' -> 14
        'E' -> 14
        'f' -> 15
        'F' -> 15
        _ -> 0
