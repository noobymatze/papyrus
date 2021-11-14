module Element exposing (..)

import Html exposing (Html)
import Html.Attributes
import Json.Encode as Encode exposing (Value)



-- ELEMENT


type Element
    = Node { name : String, attributes : List Attribute, children : List Element }
    | Text String


type alias Attribute =
    { key : String
    , value : String
    }



-- PUBLIC HELPERS


attribute : String -> String -> Attribute
attribute name value =
    { key = name, value = value }


node : String -> List Attribute -> List Element -> Element
node name attributes children =
    Node
        { name = name
        , attributes = attributes
        , children = children
        }


str : String -> Element
str =
    Text


toHtml : Element -> Html msg
toHtml element =
    case element of
        Node record ->
            Html.node record.name (List.map (\a -> Html.Attributes.attribute a.key a.value) record.attributes) (List.map toHtml record.children)

        Text string ->
            Html.text string



-- SERIALIZATION


encode : Element -> Value
encode html =
    case html of
        Node record ->
            Encode.object
                [ ( "name", Encode.string record.name )
                , ( "attributes", Encode.list encodeAttr record.attributes )
                , ( "children", Encode.list encode record.children )
                ]

        Text string ->
            Encode.string string


encodeAttr : Attribute -> Value
encodeAttr attr =
    Encode.object
        [ ( "key", Encode.string attr.key )
        , ( "value", Encode.string attr.value )
        ]
