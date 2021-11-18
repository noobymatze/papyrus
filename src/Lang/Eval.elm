module Lang.Eval exposing (..)

import Dict exposing (Dict)
import Element exposing (Attribute, Element(..), attribute, node)
import Lang.Environment as Env exposing (Environment)
import Lang.Expr as Expr exposing (Expr(..))


type Error
    = UnknownSymbol String
    | TypeMismatch { required : String, found : String }
    | Uncallable Expr
    | ArithmeticError String



-- INTERPRET


eval : Expr -> Result Error Expr
eval expr =
    expr
        |> evalHelp Env.empty
        |> Result.map Tuple.first


evalHelp : Environment -> Expr -> Result Error ( Expr, Environment )
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

        Keyword _ ->
            Ok ( expr, env )

        Map dict ->
            evalMap env dict
                |> Result.map (\new -> ( Map new, env ))

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


apply : Environment -> Expr -> List Expr -> Result Error ( Expr, Environment )
apply env proc args =
    if isBuiltin proc then
        applyBuiltin env proc args

    else
        case evalHelp env proc of
            Err error ->
                Err error

            Ok ( procedure, nextEnv ) ->
                applyFn nextEnv procedure args


applyFn : Environment -> Expr -> List Expr -> Result Error ( Expr, Environment )
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


type alias Builtin =
    List Expr -> Result Error Expr


builtin : Dict String Builtin
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
                        |> List.map Expr.toString
                        |> String.join ""
                        |> Str
                    )
            )
        |> Dict.insert "row" (nodeHelp "div" [ attribute "class" "row mt-3" ])
        |> Dict.insert "col" (nodeHelp "div" [ attribute "class" "col-sm" ])
        |> insertNode "input" [ attribute "class" "form-control" ]
        |> insertNode "label" []
        |> insertNode "h1" []
        |> insertNode "h2" []
        |> insertNode "h3" []
        |> insertNode "h4" []
        |> insertNode "div" []
        |> Dict.insert "text"
            (\args ->
                Ok (Html (Element.str (String.join "" <| List.map Expr.toString args)))
            )


nodeHelp : String -> List Attribute -> List Expr -> Result e Expr
nodeHelp name attributes args =
    Ok <| Html <| node name attributes (List.filterMap toElement args)


insertNode : String -> List Attribute -> Dict String Builtin -> Dict String Builtin
insertNode name attributes =
    Dict.insert name (\args -> Ok <| Html <| node name attributes (List.filterMap toElement args))


applyBuiltin : Environment -> Expr -> List Expr -> Result Error ( Expr, Environment )
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


evalIf : Environment -> Expr -> Expr -> Expr -> Result Error ( Expr, Environment )
evalIf env condition then_ else_ =
    case evalHelp env condition of
        Ok ( Boolean True, _ ) ->
            evalHelp env then_

        Ok ( Boolean False, _ ) ->
            evalHelp env else_

        Err error ->
            Err error

        Ok ( result, _ ) ->
            Err (TypeMismatch { required = "Bool", found = Expr.type_ result })


evalSymbol : Environment -> String -> Result Error ( Expr, Environment )
evalSymbol env symbol =
    case Env.get symbol env of
        Nothing ->
            Err (UnknownSymbol symbol)

        Just foundExpr ->
            Ok ( foundExpr, env )


evalSequence : Environment -> List Expr -> Result Error ( List Expr, Environment )
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


evalDefn : Environment -> String -> List Expr -> Expr -> List Expr -> Result Error ( Expr, Environment )
evalDefn env name args body rest =
    let
        argNames =
            List.filterMap getSymbol args
    in
    Ok
        ( Nil
        , Env.set name (Fn { args = argNames, body = body }) env
        )


evalMap : Environment -> Dict String Expr -> Result Error (Dict String Expr)
evalMap env =
    let
        combine k expr result =
            case result of
                Err error ->
                    Err error

                Ok newDict ->
                    case evalHelp env expr of
                        Err error ->
                            Err error

                        Ok ( evaled, _ ) ->
                            Ok (Dict.insert k evaled newDict)
    in
    Dict.foldl combine (Ok Dict.empty)


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
                    , found = Expr.type_ expr1 ++ " and " ++ Expr.type_ expr2
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



-- PUBLIC HELPERS


getSymbol : Expr -> Maybe String
getSymbol expr =
    case expr of
        Symbol name ->
            Just name

        _ ->
            Nothing
