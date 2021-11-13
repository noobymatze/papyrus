module Data.Widget exposing (..)

-- WIDGET


type Widget
    = Row (List Widget)
    | Column (List Widget)
    | Paragraph String
