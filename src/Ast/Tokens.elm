module Ast.Tokens exposing (length)

import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import Maybe.Extra


length : Statement -> Int
length statement =
    case statement of
        Comment s ->
            1

        ImportStatement moduleName maybeAlias maybeExportSet ->
            List.length moduleName
                + (Maybe.Extra.unwrap 0 (\a -> 1) maybeAlias)
                + (Maybe.Extra.unwrap 0 lengthExportSet maybeExportSet)

        FunctionDeclaration name params expression ->
            1
                + (List.foldl (\e l -> l + lengthExpression e) 0 params)
                + (lengthExpression expression)

        TypeAliasDeclaration name body ->
            let
                bodyLength =
                    case body of
                        TypeRecord items ->
                            List.length items * 2

                        _ ->
                            0
            in
                1 + bodyLength

        _ ->
            0


lengthExpression : Expression -> Int
lengthExpression expression =
    0


lengthExportSet : ExportSet -> Int
lengthExportSet exportSet =
    case exportSet of
        AllExport ->
            1

        SubsetExport subset ->
            List.foldl (\set l -> l + lengthExportSet set) 0 subset

        FunctionExport name ->
            1

        TypeExport name maybeExportSet ->
            1 + (Maybe.Extra.unwrap 0 lengthExportSet maybeExportSet)
