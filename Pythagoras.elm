module Pythagoras exposing (Model, buildTree, init)
import Transform exposing (Transform)
import Collage exposing (Form, groupTransform, polygon, filled)
import Color exposing (rgb)
import Math exposing (Point, calculateAngle, calculateDistance)
import Array exposing (Array, fromList, get)

type alias Model =
  { points : List Point
  , point : Point
  , e0 : Int, e1 : Int
  , e3 : Int, e2 : Int
  }

rectangle : Float -> List (Float, Float)
rectangle factor =
  [ (-1, 1), ( 0.9, 0.85) , ( 1.1,-1.5), (-1,-1) ]
  |> List.map (\(x, y) -> (x * factor, y * factor))

init : Model
init =
  let
    factor = 30
    rect = rectangle factor
  in
    { points = rect
    , point = (factor * 0, factor * 2)
    , e0 = 0, e1 = 1
    , e3 = 3, e2 = 2
    }

getPoint : List Point -> Int -> Point
getPoint points n =
  case fromList points |> get n of
    Just a -> a
    Nothing -> (0,0)

getTransformationMatrix : Model -> Transform
getTransformationMatrix model =
  let
    p0 = getPoint model.points 0
    p1 = getPoint model.points 1
    p2 = getPoint model.points 2
    p3 = getPoint model.points 3

    (p1x, p1y) = p1
    (p2x, p2y) = p2

    bottomLength = calculateDistance p2 p3
    leftLength = calculateDistance p0 model.point
    rightLength = calculateDistance p1 model.point

    leftRatio = leftLength / bottomLength
    rightRatio = rightLength / bottomLength

    r = 90 * (pi / 180)
    bottomAngle = r - calculateAngle p3 p2 p1
    topAngle = r - calculateAngle p2 p1 p0

    leftAngle = (calculateAngle model.point p0 p1) + topAngle + bottomAngle
    rightAngle = (-(calculateAngle model.point p1 p0)) + topAngle + bottomAngle

    m1 = Transform.translation -p2x -p2y
    m2 = Transform.rotation rightAngle
    m3 = Transform.scale rightRatio
    m4 = Transform.translation p2x p2y
    m5 = Transform.translation (p1x-p2x) (p1y-p2y)
  in
    List.foldl Transform.multiply Transform.identity [m1, m2, m3, m4, m5]

buildTree' : Int -> Form -> Transform -> Transform -> List Form
buildTree' n form transformationMatrix previousMatrix =
  let
    newMatrix = Transform.multiply previousMatrix transformationMatrix
    form' = groupTransform newMatrix [form]
  in
    if n > 1
      then
        [form'] ++ buildTree' (n-1) form transformationMatrix newMatrix
      else
        [form']

buildTree : Int -> Model -> List Form
buildTree n model =
  let
    transformationMatrix = getTransformationMatrix model
    {points} = model
    form = filled (rgb 255 0 0) (polygon points)
  in
    [form] ++ buildTree' n form transformationMatrix Transform.identity
