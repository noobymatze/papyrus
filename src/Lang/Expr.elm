module Lang.Expr exposing (..)

import Dict exposing (Dict)
import Element exposing (Element)
import Json.Encode as Encode exposing (Value)
import Parser exposing ((|.), (|=), DeadEnd, Parser)
import Set



-- EXPR


type Expr
    = Int Int
    | Float Float
    | Symbol String
    | Str String
    | Boolean Bool
    | Nil
    | Map (Dict String Expr)
    | Keyword String
    | Html Element
    | List (List Expr)
    | Fn { args : List String, body : Expr }
    | Prog (List Expr)



-- PUBLIC HELPERS


toString : Expr -> String
toString expr =
    case expr of
        Int int ->
            String.fromInt int

        Float float ->
            String.fromFloat float

        Symbol string ->
            string

        Str string ->
            "\"" ++ string ++ "\""

        Map dict ->
            dict
                |> Dict.toList
                |> List.map (\( a, b ) -> a ++ " " ++ toString b)
                |> String.join " "
                |> enclose "{" "}"

        Keyword string ->
            string

        Boolean True ->
            "true"

        Boolean False ->
            "false"

        Html html ->
            "Html"

        Nil ->
            ""

        List expressions ->
            "(" ++ String.join " " (List.map toString expressions) ++ ")"

        Fn _ ->
            "Fn"

        Prog expressions ->
            String.join " " (List.map toString expressions)



-- SERIALIZATION


encode : Expr -> Value
encode expr =
    case expr of
        Int int ->
            Encode.int int

        Float float ->
            Encode.float float

        Str string ->
            Encode.string string

        Map dict ->
            Encode.dict identity encode dict

        Boolean bool ->
            Encode.bool bool

        Nil ->
            Encode.null

        Symbol string ->
            Encode.string string

        Keyword string ->
            Encode.string string

        List exprs ->
            Encode.list encode exprs

        Prog exprs ->
            Encode.list encode exprs

        Html html ->
            Element.encode html

        Fn record ->
            Encode.object
                [ ( "args", Encode.list Encode.string record.args )
                , ( "body", encode record.body )
                ]


type_ : Expr -> String
type_ expr =
    case expr of
        Int _ ->
            "Int"

        Float _ ->
            "Float"

        Symbol _ ->
            "Symbol"

        Keyword _ ->
            "Keyword"

        Map _ ->
            "Map"

        Str _ ->
            "String"

        Boolean _ ->
            "Bool"

        Nil ->
            "Nil"

        Html _ ->
            "Html"

        List _ ->
            "List"

        Fn _ ->
            "Fn"

        Prog _ ->
            "Program"



-- PARSER


parse : String -> Result (List DeadEnd) Expr
parse =
    Parser.run prog


prog : Parser Expr
prog =
    Parser.map Prog <|
        Parser.sequence
            { spaces = whitespace
            , start = ""
            , end = ""
            , item = form
            , separator = ""
            , trailing = Parser.Forbidden
            }


form : Parser Expr
form =
    Parser.oneOf
        [ str
        , boolean
        , map
        , keyword
        , symbol
        , number
        , Parser.map List list
        ]


list : Parser (List Expr)
list =
    Parser.sequence
        { spaces = whitespace
        , end = ")"
        , start = "("
        , item = Parser.lazy (\_ -> form)
        , separator = ""
        , trailing = Parser.Forbidden
        }


number : Parser Expr
number =
    Parser.number
        { float = Just Float
        , int = Just Int
        , binary = Nothing
        , hex = Nothing
        , octal = Nothing
        }


boolean : Parser Expr
boolean =
    Parser.oneOf
        [ Parser.token "true" |> Parser.map (always (Boolean True))
        , Parser.token "false" |> Parser.map (always (Boolean False))
        ]


symbol : Parser Expr
symbol =
    Parser.map Symbol <|
        Parser.variable
            { start = isSymbolChar Char.isAlpha
            , inner = isSymbolChar Char.isAlphaNum
            , reserved = Set.empty
            }


map : Parser Expr
map =
    let
        toComparable ( key, value ) =
            case key of
                Str string ->
                    Just ( string, value )

                Keyword string ->
                    Just ( string, value )

                Symbol string ->
                    Just ( string, value )

                _ ->
                    Nothing
    in
    Parser.map Map <|
        Parser.map Dict.fromList <|
            Parser.map (List.filterMap toComparable) <|
                Parser.sequence
                    { spaces = whitespace
                    , end = "}"
                    , start = "{"
                    , item =
                        Parser.lazy
                            (\_ ->
                                Parser.succeed Tuple.pair
                                    |= form
                                    |. whitespace
                                    |= form
                            )
                    , separator = ""
                    , trailing = Parser.Forbidden
                    }


keyword : Parser Expr
keyword =
    Parser.map Keyword <|
        Parser.variable
            { start = \c -> c == ':'
            , inner = isSymbolChar Char.isAlphaNum
            , reserved = Set.empty
            }


isSymbolChar : (Char -> Bool) -> Char -> Bool
isSymbolChar other c =
    let
        additional =
            [ '+', '*', '/', '-', '.', '!', '_', '?', '$', '%', '&', '=', '<', '>' ]
    in
    List.member c additional || other c


whitespace : Parser ()
whitespace =
    Parser.chompWhile isWhitespace


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\n' || c == '\t'


str : Parser Expr
str =
    Parser.succeed Str
        |. Parser.token "\""
        |= strHelp
        |. Parser.token "\""


strHelp : Parser String
strHelp =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompWhile (\c -> c /= '"')


enclose : String -> String -> String -> String
enclose a b x =
    a ++ x ++ b
