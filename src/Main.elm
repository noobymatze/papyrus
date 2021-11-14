module Main exposing (main)

import Browser
import Element exposing (Element)
import Html exposing (Html, div, h1, node, pre, span, text, textarea)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onInput)
import Json.Decode
import Json.Encode as Encode
import Lang.Eval as Eval exposing (Error(..))
import Lang.Syntax as Syntax exposing (Expr(..))
import Ports



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Flags =
    { input : Maybe String
    }


type alias Model =
    { input : String
    , expr : Maybe Expr
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        input =
            flags.input
                |> Maybe.withDefault "(row\n  (col (input))\n  (col (input)))"
    in
    ( { input = input
      , expr =
            input
                |> Syntax.parse
                |> Result.toMaybe
      }
    , Cmd.none
    )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "container-fluid h-100" ]
        [ div [ class "row p-3 h-100" ]
            [ div [ class "col-sm h-100" ] [ codeEditor Update model.input ]
            , div [ class "col-sm h-100" ]
                [ Maybe.withDefault hidden <| Maybe.map viewResult <| Maybe.map Eval.eval model.expr
                ]
            ]
        ]


viewResult : Result Error Expr -> Html Msg
viewResult result =
    case result of
        Err (UnknownSymbol name) ->
            text <| "Unknown Symbol '" ++ name ++ "'"

        Err (TypeMismatch { required, found }) ->
            text <| "Type mismatch, required '" ++ required ++ "', found '" ++ found ++ "'"

        Err (Uncallable expr) ->
            text <| "The expression " ++ Encode.encode 2 (Syntax.encode expr) ++ " is uncallable or unknown"

        Ok expr ->
            let
                elements =
                    findElements expr
            in
            if List.isEmpty elements then
                text <| Encode.encode 2 (Syntax.encode expr)

            else
                div [] (List.map Element.toHtml elements)


findElements : Expr -> List Element
findElements expr =
    case expr of
        List subExpressions ->
            List.concatMap findElements subExpressions

        Prog subExpressions ->
            List.concatMap findElements subExpressions

        Html element ->
            [ element ]

        _ ->
            []


hidden : Html msg
hidden =
    span [ style "display" "none" ] []


viewExpr : Maybe Expr -> Html Msg
viewExpr maybeExpr =
    case maybeExpr of
        Nothing ->
            text ""

        Just expr ->
            expr
                |> Syntax.encode
                |> Encode.encode 2
                |> text


codeEditor : (String -> msg) -> String -> Html msg
codeEditor toMsg value =
    node "code-editor"
        [ Html.Attributes.property "editorValue" (Encode.string value)
        , Html.Events.on "editorChanged" <|
            Json.Decode.map toMsg <|
                Json.Decode.at [ "target", "editorValue" ] Json.Decode.string
        ]
        []



-- UPDATE


type Msg
    = AddWidget
    | Update String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddWidget ->
            ( model, Cmd.none )

        Update input ->
            ( { model | input = input, expr = Syntax.parse input |> Result.toMaybe }
            , Ports.save input
            )
