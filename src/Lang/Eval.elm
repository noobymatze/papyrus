module Lang.Eval exposing (..)

import Dict exposing (Dict)
import Element exposing (Element(..), attribute, node)
import Lang.Env as Env exposing (Env)
import Lang.Syntax as Syntax exposing (Expr(..))


type Error
    = UnknownSymbol String
    | TypeMismatch { required : String, found : String }
    | Uncallable Expr
    | ArithmeticError String



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

        Boolean _ ->
            Ok ( expr, env )

        Nil ->
            Ok ( expr, env )

        Str _ ->
            Ok ( expr, env )

        Html _ ->
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

        List ((Symbol "if") :: condition :: then_ :: else_ :: _) ->
            evalIf env condition then_ else_

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


builtin : Dict String (List Expr -> Result Error Expr)
builtin =
    Dict.empty
        |> Dict.insert "+" (op add (Ok (Int 0)))
        |> Dict.insert "*" (op mult (Ok (Int 1)))
        -- |> Dict.insert "/" (op mult (Err (ArithmeticError "Cannot divide nothing")))
        -- |> Dict.insert "-" (op mult (Err (ArithmeticError "Cannot subtract from nothing")))
        |> Dict.insert "str"
            (\args ->
                Ok
                    (args
                        |> List.map toString
                        |> String.join ""
                        |> Str
                    )
            )
        |> Dict.insert "row"
            (\args ->
                Ok (Html (node "div" [ attribute "class" "row" ] (List.filterMap toElement args)))
            )
        |> Dict.insert "col"
            (\args ->
                Ok (Html (node "div" [ attribute "class" "col-sm" ] (List.filterMap toElement args)))
            )
        |> Dict.insert "input"
            (\_ ->
                Ok (Html (node "input" [ attribute "class" "form-control", attribute "type" "text" ] []))
            )
        |> Dict.insert "label"
            (\args ->
                Ok (Html (node "label" [] (List.filterMap toElement args)))
            )
        |> Dict.insert "text"
            (\args ->
                Ok (Html (Element.str (String.join "" <| List.map toString args)))
            )


applyBuiltin : Env -> Expr -> List Expr -> Result Error ( Expr, Env )
applyBuiltin env expr args =
    case expr of
        Symbol name ->
            case Dict.get name builtin of
                Nothing ->
                    Err (Uncallable expr)

                Just f ->
                    f args
                        |> Result.map (\result -> ( result, env ))

        _ ->
            Err (Uncallable expr)


isBuiltin : Expr -> Bool
isBuiltin expr =
    case expr of
        Symbol symbol ->
            Dict.member symbol builtin

        _ ->
            False


evalIf : Env -> Expr -> Expr -> Expr -> Result Error ( Expr, Env )
evalIf env condition then_ else_ =
    case evalHelp env condition of
        Ok ( Boolean True, _ ) ->
            evalHelp env then_

        Ok ( Boolean False, _ ) ->
            evalHelp env else_

        Err error ->
            Err error

        Ok ( result, _ ) ->
            Err (TypeMismatch { required = "Bool", found = Syntax.type_ result })


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


op : (Expr -> Expr -> Result Error Expr) -> Result Error Expr -> List Expr -> Result Error Expr
op combine zero =
    let
        step next result =
            case result of
                Ok expr ->
                    combine expr next

                Err error ->
                    Err error
    in
    List.foldl step zero


add : Expr -> Expr -> Result Error Expr
add =
    numOp (+) (+)


mult : Expr -> Expr -> Result Error Expr
mult =
    numOp (*) (*)


numOp : (Int -> Int -> Int) -> (Float -> Float -> Float) -> Expr -> Expr -> Result Error Expr
numOp forInt forFloat expr1 expr2 =
    case ( expr1, expr2 ) of
        ( Int a, Int b ) ->
            Ok (Int (forInt a b))

        ( Int a, Float b ) ->
            Ok (Float (forFloat (toFloat a) b))

        ( Float _, Int _ ) ->
            numOp forInt forFloat expr2 expr1

        ( Float a, Float b ) ->
            Ok (Float (forFloat a b))

        ( _, _ ) ->
            Err
                (TypeMismatch
                    { required = "Int or Float"
                    , found = Syntax.type_ expr1 ++ " and " ++ Syntax.type_ expr2
                    }
                )


toElement : Expr -> Maybe Element
toElement expr =
    case expr of
        Html html ->
            Just html

        Str string ->
            Just (Text string)

        Int i ->
            Just (Text (String.fromInt i))

        Float i ->
            Just (Text (String.fromFloat i))

        _ ->
            Nothing


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



-- PUBLIC HELPERS


getSymbol : Expr -> Maybe String
getSymbol expr =
    case expr of
        Symbol name ->
            Just name

        _ ->
            Nothing
