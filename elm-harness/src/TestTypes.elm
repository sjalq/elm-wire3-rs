module TestTypes exposing (..)

{-| Test types covering all Wire3-encodable patterns.
    These are used for round-trip testing between Elm and Rust.
-}

import Dict exposing (Dict)
import Set exposing (Set)


-- Simple type aliases

type alias UserId =
    Int


type alias Score =
    Float


-- Simple enum (no params)

type Color
    = Red
    | Green
    | Blue


-- Enum with params

type Shape
    = Circle Float
    | Rectangle Float Float
    | Point


-- Record type alias

type alias Person =
    { name : String
    , age : Int
    , score : Float
    , active : Bool
    }


-- Record with containers

type alias Inventory =
    { items : List String
    , counts : Dict String Int
    , tags : Set String
    , selected : Maybe String
    }


-- Nested types

type alias Team =
    { leader : Person
    , members : List Person
    , color : Color
    }


-- Result type

type alias ApiResponse =
    { result : Result String Int
    , message : String
    }


-- Parameterized type

type Container a
    = Item a
    | Empty


-- Recursive type

type Tree
    = Leaf Int
    | Branch Tree Tree


-- Tuple fields

type alias Coordinate =
    { point : ( Float, Float )
    , label : String
    }


-- Complex nested containers

type alias Dashboard =
    { userScores : Dict String (List Int)
    , optionalData : Maybe (List String)
    , nested : Result String (Maybe Int)
    }
