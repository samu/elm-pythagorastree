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

rotateAroundPoint : Float -> Point -> Form -> Form
rotateAroundPoint angle (x, y) form =
  let
    step1 = move (-x, -y) form
    step2 = rotate angle (group [step1])
    step3 = scale 0.71 step2
    step4 = move (x, y) step3

    t1 = Transform.translation -x -y
    t2 = Transform.rotation angle
    -- t2 = Transform.identity
    t3 = Transform.translation x y
    -- t3 = Transform.identity

    r1 = Transform.multiply t1 t2
    r2 = Transform.multiply r1 t3

    f1 = groupTransform t1 [form]
    f2 = groupTransform t2 [f1]
    f3 = groupTransform t3 [f2]

    bla = groupTransform r2 [form]
  in
    -- f3
    bla
    -- step4
    -- step2
    -- groupTransform r2 [form]
