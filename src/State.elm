module State exposing (update, subscriptions)

import Array
import Data.Model exposing (Model)
import Messages exposing (Msg(..), MoveCursor(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            updatePure msg model
    in
        ( newModel
        , Cmd.none
        )


updatePure : Msg -> Model -> Model
updatePure msg model =
    let
        astArray =
            Array.fromList model.ast

        maybeLine =
            case model.cursor of
                line :: _ ->
                    Array.get line astArray

                _ ->
                    Nothing

        numberOfLines =
            List.length model.ast

        numberOfTokens =
            case maybeLine of
                Just line ->
                    3

                Nothing ->
                    0
    in
        case msg of
            MoveCursor move ->
                { model
                    | cursor =
                        moveCursor move
                            model.cursor
                            numberOfLines
                            numberOfTokens
                }


moveCursor : MoveCursor -> List Int -> Int -> Int -> List Int
moveCursor msg cursor numberOfLines numberOfTokens =
    case msg of
        Up ->
            case cursor of
                line :: _ ->
                    if line > 0 then
                        [ line - 1 ]
                    else
                        [ 0 ]

                _ ->
                    [ 0 ]

        Down ->
            case cursor of
                line :: _ ->
                    if line < numberOfLines then
                        [ line + 1 ]
                    else
                        [ numberOfLines ]

                _ ->
                    [ numberOfLines ]

        Left ->
            case cursor of
                line :: token :: rest ->
                    if token > 0 then
                        [ line, token - 1 ] ++ rest
                    else
                        [ line, token ] ++ rest

                _ ->
                    cursor ++ [ numberOfTokens ]

        Right ->
            case cursor of
                line :: token :: rest ->
                    if token < numberOfTokens then
                        [ line, token + 1 ] ++ rest
                    else
                        [ line, token ] ++ rest

                _ ->
                    cursor ++ [ 0 ]

        Next ->
            cursor


subscriptions model =
    Sub.none
