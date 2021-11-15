module Lang.Environment exposing
    ( Environment
    , empty, extend
    , get, set
    )

{-| An environment for evaluating [`Expr`].


# Environment

@docs Environment


# Creating environments

@docs empty, extend


# Working with environments

@docs get, set

-}

import Dict exposing (Dict)
import Lang.Expr exposing (Expr)



-- ENVIRONMENT


{-| An environment contains all functions and variables, that are available in a given scope.
-}
type Environment
    = Env
        { bindings : Dict String Expr
        , parent : Maybe Environment
        }



-- CREATING ENVIRONMENTS


{-| Returns an empty [`Environment`].
-}
empty : Environment
empty =
    Env { bindings = Dict.empty, parent = Nothing }


{-| Extend the given environment with the given bindings.
-}
extend : Dict String Expr -> Environment -> Environment
extend bindings parent =
    Env { bindings = bindings, parent = Just parent }



-- WORKING WITH ENVIRONMENTS


{-| Sets the given value in the given environment.
-}
set : String -> Expr -> Environment -> Environment
set name expr (Env env) =
    Env { env | bindings = Dict.insert name expr env.bindings }


{-| Try to find the given name in the environment.
-}
get : String -> Environment -> Maybe Expr
get name (Env env) =
    case Dict.get name env.bindings of
        Nothing ->
            env.parent
                |> Maybe.andThen (\(Env parentEnv) -> Dict.get name parentEnv.bindings)

        Just expr ->
            Just expr
