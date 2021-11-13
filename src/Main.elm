module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, node, pre, span, text, textarea)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onInput)
import Json.Decode
import Json.Encode as Encode
import Lang.Html as Html exposing (compile)
import Lang.Syntax as Syntax exposing (Expr)
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
            , div [ class "col-sm h-100" ] [ Maybe.withDefault hidden (Maybe.andThen Html.compile model.expr) ]
            ]
        ]


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
