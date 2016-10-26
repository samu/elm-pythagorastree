module Math exposing (..)

import Collage exposing (Form, group, move, rotate, scale, groupTransform)
import Transform exposing (Transform)


type alias Point =
    ( Float, Float )


calculateDistance : Point -> Point -> Float
calculateDistance ( from_x, from_y ) ( to_x, to_y ) =
    let
        a =
            (to_x - from_x)

        b =
            (to_y - from_y)
    in
        sqrt (a * a + b * b)


calculateAngle : Point -> Point -> Point -> Float
calculateAngle p1 p2 p3 =
    let
        a =
            calculateDistance p1 p2

        b =
            calculateDistance p2 p3

        c =
            calculateDistance p1 p3
    in
        acos ((a * a + b * b - c * c) / (2 * a * b))


calculateClosestPoint : ( Point, Point ) -> Point -> Point
calculateClosestPoint ( ( x1, y1 ), ( x2, y2 ) ) ( x3, y3 ) =
    let
        xDelta =
            x2 - x1

        yDelta =
            y2 - y1

        u =
            ((x3 - x1) * xDelta + (y3 - y1) * yDelta) / (xDelta * xDelta + yDelta * yDelta)
    in
        if u < 0 then
            ( x1, y1 )
        else if u > 1 then
            ( x2, y2 )
        else
            ( x1 + u * xDelta, y1 + u * yDelta )


calculateCrossProduct : Point -> Point -> Point -> Float
calculateCrossProduct ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) =
    (x1 - x0) * (y2 - y1) - (y1 - y0) * (x2 - x1)


liesInPolygon : Point -> List Point -> Bool
liesInPolygon point polygon =
    let
        l1 =
            polygon

        l2 =
            (List.drop 1 polygon) ++ (List.take 1 polygon)
    in
        List.map2 (,) l1 l2
            |> List.all (\( p1, p2 ) -> (calculateCrossProduct p1 p2 point) < 0)
