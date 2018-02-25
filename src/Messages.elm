module Messages exposing (..)


type Msg
    = MoveCursor MoveCursor


type MoveCursor
    = Up
    | Down
    | Left
    | Right
    | Next
