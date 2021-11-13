module Lang.Env exposing (..)

import Dict exposing (Dict)
import Lang.Syntax exposing (Expr)



-- ENV


type Env
    = Env
        { bindings : Dict String Expr
        , parent : Maybe Env
        }



-- PUBLIC HELPERS


default : Env
default =
    Env { bindings = Dict.empty, parent = Nothing }


withParent : Env -> Dict String Expr -> Env
withParent parent bindings =
    Env { bindings = bindings, parent = Just parent }


set : String -> Expr -> Env -> Env
set name expr (Env env) =
    Env { env | bindings = Dict.insert name expr env.bindings }


get : String -> Env -> Maybe Expr
get name (Env env) =
    case Dict.get name env.bindings of
        Nothing ->
            env.parent
                |> Maybe.andThen (\(Env parentEnv) -> Dict.get name parentEnv.bindings)

        Just expr ->
            Just expr
