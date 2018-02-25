module Main exposing (..)

import Html exposing (program)
import Data.Model exposing (init)
import State exposing (update, subscriptions)
import Views.Main exposing (view)


-- PROGRAM


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
