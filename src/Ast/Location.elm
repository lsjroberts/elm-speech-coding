module Ast.Location exposing (at)

import Array
import Ast.Statement exposing (..)


at : List Statement -> List Int -> List Statement
at statements location =
    let
        root =
            Array.fromList statements
    in
        List.foldl
            (\l tree ->
                case tree.current of
                    Just current ->
                        case Array.get l current of
                            Just statement ->
                                statement :: tree

                            Nothing ->
                                tree
            )
            { root = root, current = root }
            location


line : List Statement -> Int -> Maybe Statement
line statements index =
    statements |> Array.fromList |> Array.get index


expression : Statement -> Int -> Maybe Expression
expression s index =
    case s of
        Comment string ->
            if index == 0 then
                Just string
            else
                Nothing

        FunctionDeclaration name params e ->
            case index of
                0 ->
                    Just name

                1 ->
                    Just params

                2 ->
                    Just e

                3 ->
                    Nothing

        _ ->
            Nothing
