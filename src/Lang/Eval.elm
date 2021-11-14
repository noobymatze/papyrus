module Lang.Eval exposing (..)

import Dict
import Lang.Env as Env exposing (Env)
import Lang.Syntax exposing (Expr(..))


type Error
    = UnknownSymbol String
    | TypeMismatch { required : String, found : String }
    | Uncallable Expr



-- INTERPRET


eval : Expr -> Result Error Expr
eval expr =
    expr
        |> evalHelp Env.default
        |> Result.map Tuple.first


evalHelp : Env -> Expr -> Result Error ( Expr, Env )
evalHelp env expr =
    case expr of
        Int _ ->
            Ok ( expr, env )

        Float _ ->
            Ok ( expr, env )

        Nil ->
            Ok ( expr, env )

        Str _ ->
            Ok ( expr, env )

        Symbol symbol ->
            evalSymbol env symbol

        Fn _ ->
            Ok ( expr, env )

        List [] ->
            Ok ( expr, env )

        List ((Symbol "def") :: (Symbol name) :: body :: rest) ->
            case evalHelp env body of
                Err error ->
                    Err error

                Ok ( result, _ ) ->
                    Ok
                        ( result
                        , Env.set name result env
                        )

        List ((Symbol "defn") :: (Symbol name) :: (List args) :: body :: rest) ->
            evalDefn env name args body rest

        List (proc :: params) ->
            case evalSequence env params of
                Err error ->
                    Err error

                Ok ( args, procEnv ) ->
                    apply procEnv proc args

        Prog expressions ->
            expressions
                |> evalSequence env
                |> Result.map (Tuple.mapFirst List)


apply : Env -> Expr -> List Expr -> Result Error ( Expr, Env )
apply env proc args =
    if isBuiltin proc then
        applyBuiltin env proc args

    else
        case evalHelp env proc of
            Err error ->
                Err error

            Ok ( procedure, nextEnv ) ->
                applyFn nextEnv procedure args


applyFn : Env -> Expr -> List Expr -> Result Error ( Expr, Env )
applyFn env expr params =
    case expr of
        Fn { args, body } ->
            let
                mapped =
                    params
                        |> List.map2 Tuple.pair args
                        |> Dict.fromList
            in
            evalHelp (Env.extend mapped env) body

        _ ->
            Err (Uncallable expr)


applyBuiltin : Env -> Expr -> List Expr -> Result Error ( Expr, Env )
applyBuiltin env expr args =
    case expr of
        Symbol "str" ->
            Ok
                ( args
                    |> List.map str
                    |> String.join ""
                    |> Str
                , env
                )

        _ ->
            Err (Uncallable expr)


isBuiltin : Expr -> Bool
isBuiltin expr =
    case expr of
        Symbol symbol ->
            List.member symbol [ "+", "-", "*", "/", "str" ]

        _ ->
            False


evalSymbol : Env -> String -> Result Error ( Expr, Env )
evalSymbol env symbol =
    case Env.get symbol env of
        Nothing ->
            Err (UnknownSymbol symbol)

        Just foundExpr ->
            Ok ( foundExpr, env )


evalSequence : Env -> List Expr -> Result Error ( List Expr, Env )
evalSequence env =
    let
        evalSequenceHelp ( result, curEnv ) expressions =
            case expressions of
                [] ->
                    Ok ( result, curEnv )

                next :: rest ->
                    case evalHelp curEnv next of
                        Err error ->
                            Err error

                        Ok ( expr, nextEnv ) ->
                            evalSequenceHelp ( result ++ [ expr ], nextEnv ) rest
    in
    evalSequenceHelp ( [], env )


evalDefn : Env -> String -> List Expr -> Expr -> List Expr -> Result Error ( Expr, Env )
evalDefn env name args body rest =
    let
        argNames =
            List.filterMap getSymbol args
    in
    Ok
        ( Nil
        , Env.set name (Fn { args = argNames, body = body }) env
        )


add : Expr -> Expr -> Result Error Expr
add expr1 expr2 =
    case ( expr1, expr2 ) of
        ( Int a, Int b ) ->
            Ok (Int (a + b))

        ( Int a, Float b ) ->
            Ok (Float (toFloat a + b))

        ( Float _, Int _ ) ->
            add expr2 expr1

        ( Float a, Float b ) ->
            Ok (Float (a + b))

        ( _, _ ) ->
            Err (TypeMismatch { required = "Int or Float", found = "Something else" })


str : Expr -> String
str expr =
    case expr of
        Int int ->
            String.fromInt int

        Float float ->
            String.fromFloat float

        Symbol string ->
            string

        Str string ->
            string

        Nil ->
            ""

        List expressions ->
            "(" ++ String.join " " (List.map str expressions) ++ ")"

        Fn _ ->
            "Fn"

        Prog expressions ->
            String.join " " (List.map str expressions)



-- PUBLIC HELPERS


getSymbol : Expr -> Maybe String
getSymbol expr =
    case expr of
        Symbol name ->
            Just name

        _ ->
            Nothing