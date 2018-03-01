module Views.Main exposing (view)

import Array
import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import Ast.Stringify
import Color
import Element exposing (..)
import Element.Attributes as Attrs
import Element.Events as Events
import Style
import Style.Border
import Style.Color
import Style.Font
import Messages exposing (Msg(..), MoveCursor(..))


type Styles
    = None
    | Editor
    | Statement
    | Token
    | CommandButton


type Variations
    = Highlighted


stylesheet =
    Style.styleSheet
        [ Style.style None []
        , Style.style Editor
            [ Style.Color.background (Color.rgb 41 44 63)
            , Style.Color.text (Color.rgb 191 194 214)
            , Style.Font.typeface [ Style.Font.monospace ]
            , Style.Font.size 16
            ]
        , Style.style Statement
            [ Style.variation Highlighted
                [ Style.Color.background (Color.rgb 56 61 82)
                ]
            ]
        , Style.style Token
            [ Style.Border.bottom 2
            , Style.Color.border (Color.rgba 0 0 0 0)
            , Style.variation Highlighted
                [ Style.Color.border (Color.rgb 126 83 197)
                ]
            ]
        , Style.style CommandButton
            [ Style.Border.all 1
            , Style.Border.rounded 2
            , Style.Color.background (Color.rgb 230 230 230)
            , Style.hover
                [ Style.Color.background (Color.rgb 240 240 240) ]
            ]
        ]


view model =
    Element.layout stylesheet <|
        row None
            [ Attrs.width Attrs.fill ]
            [ column None
                [ Attrs.width (Attrs.percent 80)
                , Attrs.spacing 16
                ]
                [ viewAst model.cursor model.ast
                ]
            , column None
                [ Attrs.padding 32 ]
                [ el None
                    []
                    (model.cursor
                        |> List.map toString
                        |> String.join "."
                        |> text
                    )
                , button CommandButton
                    [ Attrs.paddingXY 4 2
                    , Events.onClick (MoveCursor Up)
                    ]
                  <|
                    text "Up"
                , button CommandButton
                    [ Attrs.paddingXY 4 2
                    , Events.onClick (MoveCursor Down)
                    ]
                  <|
                    text "Down"
                , button CommandButton
                    [ Attrs.paddingXY 4 2
                    , Events.onClick (MoveCursor Left)
                    ]
                  <|
                    text "Left"
                , button CommandButton
                    [ Attrs.paddingXY 4 2
                    , Events.onClick (MoveCursor Right)
                    ]
                  <|
                    text "Right"
                ]
            ]


viewAst cursor ast =
    column Editor [ Attrs.padding 32 ] (List.indexedMap (viewLine cursor) ast)


viewLine cursor index line =
    let
        cursorLineNumber =
            case cursor of
                l :: _ ->
                    l

                [] ->
                    0

        restCursor =
            case List.tail cursor of
                Just t ->
                    t

                Nothing ->
                    []
    in
        el Statement
            [ Attrs.padding 16
            , Attrs.vary Highlighted (cursorLineNumber == index)
            ]
            (viewStatement restCursor (cursorLineNumber == index) line)


viewStatement : List Int -> Bool -> Statement -> Element Styles Variations msg
viewStatement cursor isLineHighlighted statement =
    let
        viewTokenH =
            viewToken cursor isLineHighlighted
    in
        case statement of
            Comment string ->
                text ("--" ++ string)

            ImportStatement moduleName maybeAlias maybeExportSet ->
                row None [] <|
                    [ text "import " ]
                        ++ (moduleName
                                |> List.indexedMap
                                    (\moduleIndex name ->
                                        viewTokenH 0 moduleIndex (text name)
                                    )
                                |> List.intersperse (text ".")
                           )

            FunctionDeclaration name params expression ->
                let
                    paramsElements =
                        params
                            |> List.indexedMap
                                (\paramIndex param ->
                                    viewTokenH 0 (paramIndex + 1) (viewExpression 1 expression)
                                )
                in
                    column None [] <|
                        [ row None
                            []
                            (List.concat
                                [ [ (viewTokenH 0 0 (text name)) ]
                                , paramsElements
                                , [ text " =" ]
                                ]
                            )
                        , viewExpression 1 expression
                        ]

            ModuleDeclaration moduleName set ->
                row None
                    []
                    [ text "module "
                    , viewTokenH 0 0 <| text (String.join "." moduleName)
                    , text " exposing ("
                    , viewExportSet (viewTokenH 0 1) set
                    , text ")"
                    ]

            TypeDeclaration name types ->
                column None
                    [ Attrs.spacing 4 ]
                    [ row None
                        []
                        [ text "type "
                        , viewTokenH 0 0 <| text (typeString name)
                        ]
                    , column None [ Attrs.spacing 4 ] <|
                        List.indexedMap
                            (\typeIndex t ->
                                if typeIndex == 0 then
                                    row None
                                        []
                                        [ text "  = "
                                        , viewTokenH 0 (typeIndex + 1) <| text (typeString t)
                                        ]
                                else
                                    row None
                                        []
                                        [ text "  | "
                                        , viewTokenH 0 (typeIndex + 1) <| text (typeString t)
                                        ]
                            )
                            types
                    ]

            TypeAliasDeclaration name body ->
                column None
                    [ Attrs.spacing 4 ]
                    [ row None
                        []
                        [ text "type alias "
                        , viewTokenH 0 0 <| text (typeString name)
                        , text " ="
                        ]
                    , column None
                        [ Attrs.paddingLeft 16, Attrs.spacing 4 ]
                        (case body of
                            TypeRecord items ->
                                (List.indexedMap
                                    (\itemIndex ( name, t ) ->
                                        let
                                            prefix =
                                                if itemIndex == 0 then
                                                    text "{ "
                                                else
                                                    text ", "
                                        in
                                            row None
                                                []
                                                [ prefix
                                                , viewTokenH 0 (itemIndex * 2 + 1) <| text name
                                                , text " : "
                                                , viewTokenH 0 (itemIndex * 2 + 2) <| text (typeString t)
                                                ]
                                    )
                                    items
                                )
                                    ++ [ text "}" ]

                            _ ->
                                [ unrecognised "TypeAliasDeclaration body" "" ]
                        )
                    ]

            -- TypeRecord items ->
            --     let
            --         itemStrings =
            --             items
            --                 |> List.map (\( name, t ) -> name ++ " : " ++ (typeString t))
            --                 |> String.join "\n  , "
            --     in
            --         "{ " ++ itemStrings ++ "\n  }"
            _ ->
                unrecognised "statement" statement


viewExpression : Int -> Expression -> Element Styles Variations msg
viewExpression indent expression =
    let
        paddingIndent =
            Attrs.paddingLeft (toFloat <| indent * 8 * 4)

        indented =
            viewExpression 1

        indented2 =
            viewExpression 2
    in
        case expression of
            Application variable record ->
                column None
                    [ paddingIndent ]
                    [ viewExpression 0 variable
                    , viewExpression 1 record
                    ]

            Record items ->
                let
                    itemsElements =
                        items
                            |> List.indexedMap
                                (\index ( name, expression ) ->
                                    row None
                                        []
                                    <|
                                        (if index == 0 then
                                            [ text "{ ", text name ]
                                         else
                                            [ text ", ", text name ]
                                        )
                                            ++ [ text " = "
                                               , (viewExpression 1 expression)
                                               ]
                                )
                in
                    column None [ paddingIndent ] <| itemsElements ++ [ text "}" ]

            Variable names ->
                text <| String.join "" names

            _ ->
                unrecognised "expression" expression


viewExportSet viewToken set =
    case set of
        AllExport ->
            viewToken (text "..")

        _ ->
            unrecognised "exportSet" set


viewToken : List Int -> Bool -> Int -> Int -> Element Styles Variations msg -> Element Styles Variations msg
viewToken cursor isLineHighlighted indexPosition index element =
    let
        indices =
            Array.fromList cursor

        maybeCursorAtPosition =
            Array.get indexPosition indices

        isHighlighted =
            case maybeCursorAtPosition of
                Just cursorAtPosition ->
                    isLineHighlighted && cursorAtPosition == index

                Nothing ->
                    False
    in
        el Token [ Attrs.vary Highlighted isHighlighted ] element


unrecognised label a =
    text <|
        "<UNRECOGNISED "
            ++ label
            ++ ">\n  "
            ++ (toString a |> String.split "(" |> String.join "\n    (")
            ++ "\n</UNRECOGNISED>"


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
            "unrecognised type"



-- --


cursorNode ast cursor =
    let
        lineNumber =
            case cursor of
                l :: _ ->
                    l

                [] ->
                    0

        line =
            Array.fromList ast |> Array.get lineNumber
    in
        toString line
