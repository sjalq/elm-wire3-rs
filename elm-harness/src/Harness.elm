port module Harness exposing (main)

{-| Wire3 round-trip test harness.

Protocol (line-based over stdin/stdout via ports):
  Decode+re-encode: "<typeName> <hex>"  →  "OK <hex>" | "ERR <msg>"
  Elm encodes:      "ENCODE <typeName> <caseId>"  →  "OK <hex>" | "ERR <msg>"

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
        "ENCODE" :: typeName :: caseId :: _ ->
            handleEncode typeName caseId

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


-- ── ENCODE command: Elm creates values and encodes them ──────


handleEncode : String -> String -> String
handleEncode typeName caseId =
    case typeName of
        "Int" ->
            encodeCase Wire3.encodeInt intFixtures caseId

        "Float" ->
            encodeCase (BE.float64 Bytes.LE) floatFixtures caseId

        "Bool" ->
            encodeCase Wire3.encodeBool boolFixtures caseId

        "String" ->
            encodeCase Wire3.encodeString stringFixtures caseId

        "Color" ->
            encodeCase w3_encode_Color colorFixtures caseId

        "Shape" ->
            encodeCase w3_encode_Shape shapeFixtures caseId

        "Person" ->
            encodeCase w3_encode_Person personFixtures caseId

        "Inventory" ->
            encodeCase w3_encode_Inventory inventoryFixtures caseId

        "ApiResponse" ->
            encodeCase w3_encode_ApiResponse apiResponseFixtures caseId

        "Tree" ->
            encodeCase w3_encode_Tree treeFixtures caseId

        "Coordinate" ->
            encodeCase w3_encode_Coordinate coordinateFixtures caseId

        "Dashboard" ->
            encodeCase w3_encode_Dashboard dashboardFixtures caseId

        "Team" ->
            encodeCase w3_encode_Team teamFixtures caseId

        _ ->
            "ERR unknown-encode-type:" ++ typeName


encodeCase : (a -> BE.Encoder) -> List ( String, a ) -> String -> String
encodeCase encoder fixtures caseId =
    case List.filter (\( id, _ ) -> id == caseId) fixtures |> List.head of
        Just ( _, value ) ->
            "OK " ++ bytesToHex (BE.encode (encoder value))

        Nothing ->
            "ERR unknown-case:" ++ caseId


-- ── Test fixtures (predefined values for each type) ──────────


intFixtures : List ( String, Int )
intFixtures =
    [ ( "0", 0 )
    , ( "1", 1 )
    , ( "neg1", -1 )
    , ( "42", 42 )
    , ( "boundary1", 107 )
    , ( "boundary2", 108 )
    , ( "boundary3", 4715 )
    , ( "boundary4", 4716 )
    , ( "large", 8388608 )
    , ( "max", 9007199254740991 )
    , ( "min", -9007199254740991 )
    , ( "neg_large", -2147483648 )
    ]


floatFixtures : List ( String, Float )
floatFixtures =
    [ ( "0", 0.0 )
    , ( "1", 1.0 )
    , ( "neg1", -1.0 )
    , ( "pi", 3.14159265358979 )
    , ( "tiny", 1.0e-10 )
    , ( "big", 1.0e10 )
    ]


boolFixtures : List ( String, Bool )
boolFixtures =
    [ ( "true", True )
    , ( "false", False )
    ]


stringFixtures : List ( String, String )
stringFixtures =
    [ ( "empty", "" )
    , ( "hello", "hello" )
    , ( "unicode", "héllo wörld" )
    , ( "cjk", "日本語テスト" )
    , ( "emoji", "🎉🚀🌍" )
    , ( "newlines", "line1\nline2\ttab" )
    ]


colorFixtures : List ( String, Color )
colorFixtures =
    [ ( "red", Red )
    , ( "green", Green )
    , ( "blue", Blue )
    ]


shapeFixtures : List ( String, Shape )
shapeFixtures =
    [ ( "circle", Circle 5.0 )
    , ( "point", Point )
    , ( "rect", Rectangle 3.0 4.0 )
    ]


personFixtures : List ( String, Person )
personFixtures =
    [ ( "alice", { name = "Alice", age = 30, score = 95.5, active = True } )
    , ( "empty", { name = "", age = 0, score = 0.0, active = False } )
    , ( "unicode", { name = "José García 🏠", age = 99, score = -1.5, active = True } )
    ]


inventoryFixtures : List ( String, Inventory )
inventoryFixtures =
    [ ( "empty"
      , { items = [], counts = Dict.empty, tags = Set.empty, selected = Nothing }
      )
    , ( "full"
      , { items = [ "apple", "banana" ]
        , counts = Dict.fromList [ ( "apple", 3 ), ( "banana", 5 ) ]
        , tags = Set.fromList [ "fruit", "organic" ]
        , selected = Just "apple"
        }
      )
    ]


apiResponseFixtures : List ( String, ApiResponse )
apiResponseFixtures =
    [ ( "ok", { result = Ok 42, message = "success" } )
    , ( "err", { result = Err "not found", message = "failed" } )
    ]


treeFixtures : List ( String, Tree )
treeFixtures =
    [ ( "leaf", Leaf 42 )
    , ( "branch", Branch (Leaf 1) (Leaf 2) )
    , ( "nested", Branch (Branch (Leaf 10) (Leaf 20)) (Leaf 30) )
    ]


coordinateFixtures : List ( String, Coordinate )
coordinateFixtures =
    [ ( "origin", { point = ( 0.0, 0.0 ), label = "origin" } )
    , ( "point", { point = ( 100.5, 200.3 ), label = "top-right" } )
    ]


dashboardFixtures : List ( String, Dashboard )
dashboardFixtures =
    [ ( "empty"
      , { userScores = Dict.empty, optionalData = Nothing, nested = Ok Nothing }
      )
    , ( "full"
      , { userScores = Dict.fromList [ ( "alice", [ 10, 20 ] ), ( "bob", [ 30 ] ) ]
        , optionalData = Just [ "hello", "world" ]
        , nested = Ok (Just 42)
        }
      )
    , ( "error"
      , { userScores = Dict.empty
        , optionalData = Nothing
        , nested = Err "something went wrong"
        }
      )
    ]


teamFixtures : List ( String, Team )
teamFixtures =
    [ ( "small"
      , { leader = { name = "Alice", age = 30, score = 95.5, active = True }
        , members = [ { name = "Bob", age = 25, score = 80.0, active = True } ]
        , color = Blue
        }
      )
    , ( "empty"
      , { leader = { name = "Solo", age = 40, score = 100.0, active = True }
        , members = []
        , color = Red
        }
      )
    ]


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
