module Data.Model exposing (Model, init)

import Ast
import Ast.Statement exposing (Statement)


type alias Model =
    { source : String
    , ast : List Statement
    , cursor : List Int
    }


init : ( Model, Cmd msg )
init =
    ( Model source (parse source) [ 7, 2 ], Cmd.none )


source =
    """module Main exposing (..)

import Html exposing (program)
import Data.Model exposing (init)
import State exposing (update, subscriptions)
import Views.Main exposing (view)


-- PROGRAM

type Foo
    = Int Int
    | String

type alias Model =
    { x : Int
    , y : Float
    }

main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

foo : Int -> Int
foo x =
    x + 1

bar : Int -> Int -> Int
bar x y =
    (foo x) + y + 2

baz z =
    case z of
        Comment string ->
            "--" ++ string

        Thing a b c ->
            toString c

        _ ->
            "other"
"""


parse string =
    case Ast.parse string of
        Ok ( _, _, statements ) ->
            statements

        err ->
            []
