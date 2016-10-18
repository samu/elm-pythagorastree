module Pythagoras exposing (Model, buildTree, init)
import Transform exposing (Transform)
import Collage exposing (Form, groupTransform, polygon, filled)
import Color exposing (rgb)
import Math exposing (Point)
import Array exposing (Array, fromList, get)
-- import Maybe exposing (Maybe)

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
    factor = 20
    rect = rectangle factor
  in
    { points = rect
    , point = (factor * -0.3, factor * 2)
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
    (p1x, p1y) = getPoint model.points 1
    (p2x, p2y) = getPoint model.points 2
    m1 = Transform.translation -p2x -p2y
    m2 = Transform.rotation (degrees -22)
    m3 = Transform.scale 0.8
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
