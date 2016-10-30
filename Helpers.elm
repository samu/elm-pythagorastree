module Helpers exposing (..)

import Collage exposing (Form, move, filled, circle)
import Math exposing (Point)
import Color exposing (Color, rgb)
import Array


screenPointToCollage : ( Int, Int ) -> ( Int, Int ) -> Point
screenPointToCollage ( mouseX, mouseY ) ( width, height ) =
    ( screenCoordsToCollage mouseX width, -(screenCoordsToCollage mouseY height) )


screenCoordsToCollage : Int -> Int -> Float
screenCoordsToCollage screenCoord screenSize =
    (toFloat screenCoord) - ((toFloat screenSize) / 2)


drawPoint : Color -> Int -> Point -> Form
drawPoint color radius point =
    move point (filled color (circle (toFloat radius)))


colorFromList : List Int -> Color
colorFromList integers =
    case integers of
        r :: g :: b :: tail ->
            rgb r g b

        _ ->
            rgb 0 0 0
