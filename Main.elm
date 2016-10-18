import Html exposing (Html, Attribute, text, div, input)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Collage exposing (Form, groupTransform, polygon, collage, filled, rect, circle, move, rotate)
import Transform exposing (Transform)
import Color exposing (Color, rgb)
import Element exposing (toHtml)
import Window
import String
import Mouse
import Math exposing (calculateDistance, calculateAngle, rotateAroundPoint)
import Debug exposing (log)
import Pythagoras exposing (buildTree)

main =
  program { init = init, view = view, update = update, subscriptions = subscriptions }

type alias Model =
  { width : Int, height : Int
  , mouseX : Int, mouseY : Int
  , points : List (Float, Float)
  , point : (Float, Float)
  , edges : List Int
  }

rectangle : Float -> List (Float, Float)
rectangle factor =
  [ (-1, 1), ( 1, 1) , ( 1,-1), (-1,-1) ]
  |> List.map (\(x, y) -> (x * factor, y * factor))

init : (Model, Cmd Msg)
init =
  let
    factor = 20
    model =
    { width = 500, height = 500
    , mouseX = 0, mouseY = 0
    , points = rectangle factor
    , point = (factor * -0.3, factor * 2)
    , edges = [0,1,2,3]
    }
  in (model, Cmd.none)

subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes (\{height, width} -> Resize width height)
    , Mouse.moves (\{x, y} -> MouseMove x y)
    ]

type Msg
  = Update
  | Resize Int Int
  | MouseMove Int Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Update -> (model, Cmd.none)
    Resize width height ->
      ({model | width = width, height = height}, Cmd.none)
    MouseMove x y ->
      ({model | mouseX = x, mouseY = y}, Cmd.none)

drawRectangle : Color -> Int -> Int -> Form
drawRectangle color width height =
  filled color (rect (toFloat (width)) (toFloat (height)))

drawBackground : Model -> Int -> Form
drawBackground {width, height} padding =
  drawRectangle (rgb 200 200 200) (width-padding) (height-padding)

drawBaseShape : Model -> Form
drawBaseShape {points} =
  filled (rgb 255 0 0) (polygon points)

screenCoordsToCollage : Int -> Int -> Float
screenCoordsToCollage screenCoord screenSize =
  (toFloat screenCoord) - ((toFloat screenSize) / 2)

view : Model -> Html Msg
view model =
  let
    {width, height, mouseX, mouseY, point} = model
    posX = screenCoordsToCollage mouseX width
    posY = screenCoordsToCollage mouseY height

    {points} = model
    ptmodel = {points = points}

    pt = buildTree 5 ptmodel

    forms =
      [drawBackground model 0]
      ++ pt
  in
    collage width height forms |> toHtml
