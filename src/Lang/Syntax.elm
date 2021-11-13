module Lang.Syntax exposing (..)

import Html exposing (Html)
import Json.Encode as Encode exposing (Value)
import Parser exposing ((|.), (|=), DeadEnd, Parser)
import Set



-- EXPR


type Expr
    = Int Int
    | Float Float
    | Symbol String
    | Str String
    | List (List Expr)
    | Fn { args : List String, body : Expr }
    | Prog (List Expr)



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

        Symbol string ->
            Encode.string string

        List exprs ->
            Encode.list encode exprs

        Prog exprs ->
            Encode.list encode exprs

        Fn record ->
            Encode.object
                [ ( "args", Encode.list Encode.string record.args )
                , ( "body", encode record.body )
                ]



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


symbol : Parser Expr
symbol =
    Parser.map Symbol <|
        Parser.variable
            { start = isSymbolChar Char.isAlpha
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