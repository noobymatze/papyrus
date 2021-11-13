module Lang.Html exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div, input, node, text)
import Html.Attributes exposing (class, type_)
import Lang.Env as Env exposing (Env)
import Lang.Syntax as Syntax exposing (Expr)



-- COMPILE


compile : Syntax.Expr -> Maybe (Html msg)
compile expr =
    expr
        |> compileHelp Env.default
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
            case Env.get str env of
                Nothing ->
                    ( Just <| text <| "Unknown symbol " ++ str
                    , env
                    )

                Just binding ->
                    compileHelp env binding

        Syntax.List ((Syntax.Symbol "def") :: (Syntax.Symbol name) :: subExpr :: []) ->
            ( Nothing
            , Env.set name subExpr env
            )

        Syntax.List ((Syntax.Symbol "defn") :: (Syntax.Symbol name) :: (Syntax.List args) :: body :: []) ->
            ( Nothing
            , Env.set name (Syntax.Fn { args = List.filterMap symbol args, body = body }) env
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

        Syntax.List ((Syntax.Symbol fn) :: children) ->
            ( apply env fn children
            , env
            )

        Syntax.List rest ->
            ( Just <| text (Debug.toString rest)
            , env
            )

        Syntax.Fn { args, body } ->
            ( Nothing, env )

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


symbol : Expr -> Maybe String
symbol expr =
    case expr of
        Syntax.Symbol name ->
            Just name

        _ ->
            Nothing


apply : Env -> String -> List Expr -> Maybe (Html msg)
apply env name params =
    case Env.get name env of
        Nothing ->
            Just <| node name [] (List.filterMap (compileHelp env >> Tuple.first) params)

        Just (Syntax.Fn { args, body }) ->
            let
                fnEnv =
                    params
                        |> List.map2 Tuple.pair args
                        |> Dict.fromList
                        |> Env.withParent env
            in
            Tuple.first (compileHelp fnEnv body)

        Just expr ->
            Tuple.first (compileHelp env expr)
