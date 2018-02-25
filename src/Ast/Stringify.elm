module Ast.Stringify exposing (stringify, statement)

import Ast.Statement exposing (..)
import Ast.Expression exposing (..)


stringify : List Statement -> String
stringify ast =
    ast
        |> List.map statement
        |> String.join "\n\n\n"


statement : Statement -> String
statement s =
    case s of
        Comment string ->
            "--" ++ string

        FunctionDeclaration name params e ->
            name
                ++ " "
                ++ (params |> List.map expression |> String.join " ")
                ++ " =\n  "
                ++ (expression e)

        FunctionTypeDeclaration name t ->
            name ++ " : " ++ (typeString t)

        ImportStatement moduleName maybeAlias maybeExportSet ->
            "import " ++ (String.join "." moduleName)

        ModuleDeclaration moduleName set ->
            "module "
                ++ (String.join "." moduleName)
                ++ " exposing ("
                ++ (exportSet set)
                ++ ")"

        TypeAliasDeclaration name body ->
            "type alias " ++ (typeString name) ++ " =\n  " ++ (typeString body)

        TypeDeclaration name types ->
            "type "
                ++ (typeString name)
                ++ "\n  = "
                ++ (types |> List.map typeString |> String.join "\n  | ")

        _ ->
            unrecognised "statement" s


expression : Expression -> String
expression e =
    case e of
        Application variable record ->
            (expression variable) ++ " " ++ (expression record)

        Case variable body ->
            "case "
                ++ expression variable
                ++ " of\n"
                ++ (body
                        |> List.map
                            (\( e, e2 ) ->
                                "    "
                                    ++ expression e
                                    ++ " ->\n      "
                                    ++ expression e2
                            )
                        |> String.join "\n\n"
                   )

        BinOp op left right ->
            let
                appliedLeft =
                    case left of
                        Application v r ->
                            "(" ++ expression left ++ ")"

                        _ ->
                            expression left

                appliedRight =
                    case right of
                        Application v r ->
                            "(" ++ expression right ++ ")"

                        _ ->
                            expression right
            in
                String.join " " [ appliedLeft, expression op, appliedRight ]

        Character char ->
            toString char

        Float float ->
            toString float

        Integer int ->
            toString int

        Ast.Expression.List expressions ->
            "[ " ++ (expressions |> List.map expression |> String.join ", ") ++ " ]"

        Record items ->
            let
                itemStrings =
                    items
                        |> List.map (\( name, e ) -> name ++ " = " ++ (expression e))
                        |> String.join "\n    , "
            in
                "    { " ++ itemStrings ++ "\n    }"

        String string ->
            "\"" ++ string ++ "\""

        Tuple items ->
            "( " ++ (items |> List.map expression |> String.join ", ") ++ " )"

        Variable names ->
            String.join "" names

        _ ->
            unrecognised "expression" e


exportSet set =
    case set of
        AllExport ->
            ".."

        _ ->
            unrecognised "exportSet" set


typeString : Type -> String
typeString t =
    case t of
        TypeApplication left right ->
            typeString left ++ " -> " ++ typeString right

        TypeConstructor names types ->
            if List.length types > 0 then
                (String.join " " names)
                    ++ " "
                    ++ (String.join " " (List.map typeString types))
            else
                String.join " " names

        TypeRecord items ->
            let
                itemStrings =
                    items
                        |> List.map (\( name, t ) -> name ++ " : " ++ (typeString t))
                        |> String.join "\n  , "
            in
                "{ " ++ itemStrings ++ "\n  }"

        TypeVariable name ->
            toString name

        _ ->
            unrecognised "typeString" t


unrecognised label a =
    "<UNRECOGNISED " ++ label ++ ">" ++ (toString a) ++ "</UNRECOGNISED>"
