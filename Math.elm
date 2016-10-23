module Math exposing (..)
import Collage exposing (Form, group, move, rotate, scale, groupTransform)
import Transform exposing (Transform)

type alias Point = (Float, Float)

calculateDistance : Point -> Point -> Float
calculateDistance (from_x, from_y) (to_x, to_y) =
  (to_x - from_x) * (to_x - from_x) + (to_y - from_y) * (to_y - from_y)
  |> sqrt

calculateAngle : Point -> Point -> Point -> Float
calculateAngle p1 p2 p3 =
  let
    a = calculateDistance p1 p2
    b = calculateDistance p2 p3
    c = calculateDistance p1 p3
  in
    acos ((a * a + b * b - c * c) / (2 * a * b))

calculateClosestPoint : (Point, Point) -> Point -> Point
calculateClosestPoint ((x1, y1), (x2, y2)) (x3, y3) =
  let
    xDelta = x2 - x1
    yDelta = y2 - y1
    u = ((x3 - x1) * xDelta + (y3 - y1) * yDelta) / (xDelta * xDelta + yDelta * yDelta)

    closestPoint = if u < 0
      then (x1, y1)
      else if u > 1
        then (x2, y2)
        else (x1 + u * xDelta, y1 + u * yDelta)
  in closestPoint


-- BasicMath.getClosestPoint = function(line, p3) {
--   var xDelta = line.p2.x - line.p1.x;
--   var yDelta = line.p2.y - line.p1.y;
--
--   if ((xDelta == 0) && (yDelta == 0)) {
--     throw new IllegalArgumentException("line.p1 and line.p2 cannot be the same point");
--   }
--
--   var u = ((p3.x - line.p1.x) * xDelta + (p3.y - line.p1.y) * yDelta) / (xDelta * xDelta + yDelta * yDelta);
--
--   var closestPoint;
--   if (u < 0) {
--     closestPoint = line.p1;
--   } else if (u > 1) {
--     closestPoint = line.p2;
--   } else {
--     closestPoint = new Point(line.p1.x + u * xDelta, line.p1.y + u * yDelta);
--   }
--
--   return closestPoint;
-- }
