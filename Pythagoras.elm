module Pythagoras exposing (Model, buildTree, init)
import Transform exposing (Transform)
import Collage exposing (Form, groupTransform, polygon, filled)
import Color exposing (rgb)
import Math exposing (Point)

type alias Model =
  { points : List Point
  , point : Point
  , edges : List Int
  }

rectangle : Float -> List (Float, Float)
rectangle factor =
  [ (-1, 1), ( 1, 1) , ( 1,-1), (-1,-1) ]
  |> List.map (\(x, y) -> (x * factor, y * factor))

init : Model
init =
  let factor = 20
  in
    { points = rectangle factor
    , point = (factor * -0.3, factor * 2)
    , edges = [0,1,2,3]
    }

getTransformationMatrix : Model -> Transform
getTransformationMatrix model =
  let
    m1 = Transform.translation -20 20
    m2 = Transform.rotation (degrees -22)
    m3 = Transform.scale 0.8
    m4 = Transform.translation 20 -20
    m5 = Transform.translation 0 40
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
