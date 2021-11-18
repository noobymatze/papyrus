module Main exposing (main)

import Browser
import Element exposing (Element)
import Export.Wicket as Wicket
import Html exposing (Html, div, h1, node, pre, span, text, textarea)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onInput)
import Json.Decode
import Json.Encode as Encode
import Lang.Eval as Eval exposing (Error(..))
import Lang.Expr as Expr exposing (Expr(..))
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
                |> Expr.parse
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
                [ Maybe.withDefault hidden <| Maybe.map (viewResult viewExpr) <| Maybe.map Eval.eval model.expr
                , Maybe.withDefault hidden <| Maybe.map (viewResult viewWicket) <| Maybe.map Eval.eval model.expr
                ]
            ]
        ]


viewResult : (Expr -> Html Msg) -> Result Error Expr -> Html Msg
viewResult f result =
    case result of
        Err (UnknownSymbol name) ->
            text <| "Unknown Symbol '" ++ name ++ "'"

        Err (TypeMismatch { required, found }) ->
            text <| "Type mismatch, required '" ++ required ++ "', found '" ++ found ++ "'"

        Err (Uncallable expr) ->
            text <| "The expression " ++ Encode.encode 2 (Expr.encode expr) ++ " is uncallable or unknown"

        Err (ArithmeticError message) ->
            text message

        Ok expr ->
            f expr


hidden : Html msg
hidden =
    span [ style "display" "none" ] []


viewWicket : Expr -> Html Msg
viewWicket expr =
    case expr of
        Html node ->
            pre [] [ text (Wicket.compile node) ]

        List exprs ->
            div [] (List.map viewWicket exprs)

        _ ->
            hidden


viewExpr : Expr -> Html Msg
viewExpr expr =
    case expr of
        Int int ->
            text (String.fromInt int)

        Map _ ->
            text (Expr.toString expr)

        Keyword keyword ->
            text keyword

        Float float ->
            text (String.fromFloat float)

        Symbol string ->
            text string

        Str string ->
            text string

        Boolean True ->
            text "true"

        Boolean False ->
            text "false"

        Nil ->
            text "nil"

        Html element ->
            Element.toHtml element

        List exprs ->
            div [] (List.map viewExpr exprs)

        Fn _ ->
            text <| Encode.encode 2 <| Expr.encode expr

        Prog exprs ->
            div [] (List.map viewExpr exprs)


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
            ( { model | input = input, expr = Expr.parse input |> Result.toMaybe }
            , Ports.save input
            )
