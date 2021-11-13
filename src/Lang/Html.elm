module Lang.Html exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div, input, node, text)
import Html.Attributes exposing (class, type_)
import Lang.Syntax as Syntax exposing (Expr)


type alias Env =
    { bindings : Dict String Expr
    }



-- COMPILE


compile : Syntax.Expr -> Maybe (Html msg)
compile expr =
    expr
        |> compileHelp (Env Dict.empty)
        |> Tuple.first


compileHelp : Env -> Syntax.Expr -> ( Maybe (Html msg), Env )
compileHelp env expr =
    case expr of
        Syntax.Int int ->
            ( Just <| text (String.fromInt int)
            , env
            )

        Syntax.Float float ->
            ( Just <| text (String.fromFloat float)
            , env
            )

        Syntax.Symbol str ->
            case Dict.get str env.bindings of
                Nothing ->
                    ( Just <| text <| "Unknown symbol " ++ str
                    , env
                    )

                Just binding ->
                    compileHelp env binding

        Syntax.List ((Syntax.Symbol "def") :: (Syntax.Symbol name) :: subExpr :: []) ->
            ( Nothing
            , { env | bindings = Dict.insert name subExpr env.bindings }
            )

        Syntax.List ((Syntax.Symbol "row") :: children) ->
            ( Just <| div [ class "row mt-3" ] (List.filterMap (compileHelp env >> Tuple.first) children)
            , env
            )

        Syntax.List ((Syntax.Symbol "col") :: children) ->
            ( Just <| div [ class "col-sm" ] (List.filterMap (compileHelp env >> Tuple.first) children)
            , env
            )

        Syntax.List ((Syntax.Symbol "input") :: _) ->
            ( Just <| input [ type_ "text", class "form-control" ] []
            , env
            )

        Syntax.List ((Syntax.Symbol nodeName) :: children) ->
            ( Just <| node nodeName [] (List.filterMap (compileHelp env >> Tuple.first) children)
            , env
            )

        Syntax.List rest ->
            ( Just <| text (Debug.toString rest)
            , env
            )

        Syntax.Prog exprs ->
            let
                eval subExpr ( list, resultEnv ) =
                    let
                        ( html, nextEnv ) =
                            compileHelp resultEnv subExpr
                    in
                    ( list ++ [ html ], nextEnv )

                ( elements, newEnv ) =
                    List.foldl eval ( [], env ) exprs
            in
            ( Just <| div [] (List.filterMap identity elements)
            , newEnv
            )
