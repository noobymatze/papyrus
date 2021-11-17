module Export.Wicket exposing (..)

import Element exposing (Element(..))


compile : Element -> String
compile =
    compileHelp ""


compileHelp : String -> Element -> String
compileHelp indent element =
    case element of
        Node record ->
            let
                attributes =
                    record.attributes
                        |> List.map (\a -> " " ++ a.key ++ "=" ++ "\"" ++ a.value ++ "\"")
                        |> String.join ""
            in
            case record.name of
                "input" ->
                    indent ++ "<" ++ record.name ++ attributes ++ " />\n"

                _ ->
                    String.join ""
                        [ indent ++ "<" ++ record.name ++ attributes ++ ">\n"
                        , record.children |> List.map (compileHelp (indent ++ "    ")) |> String.join ""
                        , indent ++ "</" ++ record.name ++ ">\n"
                        ]

        Text string ->
            indent ++ string ++ "\n"
